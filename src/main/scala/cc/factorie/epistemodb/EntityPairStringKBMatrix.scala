package cc.factorie.epistemodb

import java.util

import com.mongodb._
import java.io.{Writer, File}
import scala.Predef._
import scala.Some
import scala.collection.mutable

/**
 * Created by beroth on 2/6/15.
 */
/**
 * Holds a knowledge-base with an underlying matrix.
 * I.e. additionally to matrix information, it also stores information about entities, relations etc.
 */



class TransEKBMatrix {
  val coocMatrix = new EntityPairStringKBMatrix

  val entityIndexMap = new MemoryIndexMap[String]

  def listEntityIndexMap: Map[Int, (Int, Int)] = {
    coocMatrix.__rowMap.keyIterator.map(key => {
      entityIndexMap.add(key.e1)
      entityIndexMap.add(key.e2)
      val id1 = entityIndexMap.keyToIndex(key.e1)
      val id2 = entityIndexMap.keyToIndex(key.e2)
      val pairId = coocMatrix.__rowMap.keyToIndex(key)
      (pairId -> (id1, id2))
    }).toMap
  }

}

/*
class TransEKBMatrix(val matrix:CoocMatrix = new CoocMatrix(0,0),
                             val __rowMap: MatrixIndexMap[EntityPair] = new EntityPairMemoryMap(collectionPrefix = MongoWritable.ENTITY_ROW_MAP_PREFIX),
                             val __colMap: MatrixIndexMap[String] = new StringMemoryIndexMap(collectionPrefix = MongoWritable.ENTITY_COL_MAP_PREFIX),
                             val entityPairMatrix: CoocMatrix = new CoocMatrix(0,0),
                             val __entityMap: MatrixIndexMap[String] = new StringMemoryIndexMap(collectionPrefix = "ENITIIES")
                              ) extends KBMatrix[EntityRelationKBMatrix, EntityPair, String]  {

  def cloneWithNewCells(cells: CoocMatrix): TransEKBMatrix = {
    new TransEKBMatrix(matrix = cells, __rowMap = this.__rowMap, __colMap = this.__colMap, entityPairMatrix = this.entityPairMatrix, __entityMap = this.__entityMap)
  }

  def createEmptyMatrix(): TransEKBMatrix = {
    new TransEKBMatrix()
  }

}
*/



class EntityPairStringKBMatrix(val matrix:CoocMatrix = new CoocMatrix(0,0),
               val __rowMap: MatrixIndexMap[EntityPair] with MongoWritable = new EntityPairMemoryMap(collectionPrefix = MongoWritable.ENTITY_ROW_MAP_PREFIX),
               val __colMap: MatrixIndexMap[String] with MongoWritable = new StringMemoryIndexMap(collectionPrefix = MongoWritable.ENTITY_COL_MAP_PREFIX)
                              ) extends KBMatrix[EntityPairStringKBMatrix, EntityPair, String] with MongoWritable {

  def cloneWithNewCells(cells: CoocMatrix): EntityPairStringKBMatrix = {
    new EntityPairStringKBMatrix(matrix = cells, __rowMap = this.__rowMap, __colMap = this.__colMap)
  }

  def createEmptyMatrix(): EntityPairStringKBMatrix = {
    new EntityPairStringKBMatrix()
  }

  def writeToMongo(mongoDb: DB) {
    matrix.writeToMongo(mongoDb)
    __rowMap.writeToMongo(mongoDb)
    __colMap.writeToMongo(mongoDb)
  }

  def populateFromMongo(mongoDb: DB) {
    matrix.populateFromMongo(mongoDb)
    __rowMap.populateFromMongo(mongoDb)
    __colMap.populateFromMongo(mongoDb)
  }
}


class StringStringKBMatrix(val matrix:CoocMatrix = new CoocMatrix(0,0),
                             val __rowMap: MatrixIndexMap[String] with MongoWritable = new StringMemoryIndexMap(collectionPrefix = MongoWritable.ENTITY_ROW_MAP_PREFIX),
                             val __colMap: MatrixIndexMap[String] with MongoWritable = new StringMemoryIndexMap(collectionPrefix = MongoWritable.ENTITY_COL_MAP_PREFIX)
                              ) extends KBMatrix[StringStringKBMatrix, String, String] with MongoWritable {

  def cloneWithNewCells(cells: CoocMatrix): StringStringKBMatrix = {
    new StringStringKBMatrix(matrix = cells, __rowMap = this.__rowMap, __colMap = this.__colMap)
  }

  def createEmptyMatrix(): StringStringKBMatrix = {
    new StringStringKBMatrix()
  }

  def writeToMongo(mongoDb: DB) {
    matrix.writeToMongo(mongoDb)
    __rowMap.writeToMongo(mongoDb)
    __colMap.writeToMongo(mongoDb)
  }

  def populateFromMongo(mongoDb: DB) {
    matrix.populateFromMongo(mongoDb)
    __rowMap.populateFromMongo(mongoDb)
    __colMap.populateFromMongo(mongoDb)
  }

  def writeToTsvFile(filename: String) {
    val pw = new java.io.PrintWriter(new File(filename))
    for (rowStr <- this.__rowMap.keyIterator) {
      for (colStr <- this.getColsForRow(rowStr)) {
        val count = this.get(rowStr, colStr)
        pw.println(f"$rowStr%s\t$colStr%s\t$count%.4f")
      }
    }
    pw.close()
  }

  def writeTopPatterns(testCols: Set[String], model: UniversalSchemaModel, threshold: Double, filename: String) {
    val pw = new java.io.PrintWriter(new File(filename))
    for (testColStr <- testCols;
    if (__colMap.containsKey(testColStr) &&
      matrix.nonZeroCols().contains(__colMap.keyToIndex(testColStr)))
    ) {
      val testColIdx = this.__colMap.keyToIndex(testColStr)
      val testColVec = model.colVectors(testColIdx)
      model.getScoredColumns(testColVec).
        filter(_._2 > threshold).
        map(idxScore => (this.__colMap.indexToKey(idxScore._1), idxScore._2)). // map col index to surface form
        filter(strScore => !testCols.contains(strScore._1)).foreach(strScore => {
        val pattern = strScore._1
        val score = strScore._2
        pw.println(f"$score%.4f\t$testColStr%s\t$pattern%s")
      })
    }
    pw.close()
  }

  def writeColumnEmbeddings(model: UniversalSchemaModel, writer: Writer, constrainTo: Option[Iterable[String]] = None,
                            dontWrite: Set[String] = Set()) {
    val colIds: Iterable[Int] = constrainTo match {
      case Some(ids) => ids.map(id => __colMap.keyToIndex(id))
      case None => Range(0, __colMap.size)
    }
    for (colId <- colIds) {
      val relStr = __colMap.indexToKey(colId)
      if (!dontWrite.contains(relStr)) {
        val vecStr = model.colVectors(colId).mkString(" ")
        writer.write(relStr + "\t" + vecStr + "\n")
      }
    }
  }
}



object StringStringKBMatrix {

  private def entitiesAndRelFromLine(line: String, colsPerEnt:Int): (String, String, Double) = {
    val parts = line.split("\t")
    if (parts.length < 2 * colsPerEnt + 2) {
      throw new IllegalArgumentException("Line specifying matrix cell needs columns for 2 entities, relation, and count.")
    }
    val ep : String = parts.slice(0, 2 * colsPerEnt).mkString("\t")
    val rel : String = parts.slice(2 * colsPerEnt, parts.length - 1).mkString("\t")
    val cellVal : Double = parts(parts.length - 1).toDouble
    (ep, rel, cellVal)
  }

  def fromTsvMongoBacked(mongoDb: DB, filename:String, colsPerEnt:Int = 2) : StringStringKBMatrix = {
    val rowMap = new StringMongoMap(mongoDb = mongoDb, collectionPrefix = MongoWritable.ENTITY_ROW_MAP_PREFIX)
    val colMap = new StringMongoMap(mongoDb = mongoDb, collectionPrefix = MongoWritable.ENTITY_COL_MAP_PREFIX)

    val kb = new StringStringKBMatrix(__rowMap = rowMap, __colMap = colMap)

    val tReadStart = System.currentTimeMillis
    var numRead = 0
    scala.io.Source.fromFile(filename).getLines.foreach(line => {
      val (ep, rel, cellVal) = entitiesAndRelFromLine(line, colsPerEnt)
      kb.set(ep, rel, cellVal)

      numRead += 1
      if (numRead % 100000 == 0) {
        val tRead = numRead / (System.currentTimeMillis - tReadStart).toDouble
        println(f"cells read per millisecond: $tRead%.4f")
        println(f"Last row: (${ep}s)")
        println(f"Last column: (${rel}s)")
        println(f"Last cell value: $cellVal%.4f")
      }
    })
    kb
  }


  def fromTsv(filename:String, colsPerEnt:Int = 2) : StringStringKBMatrix = {
    val kb = new StringStringKBMatrix()

    val tReadStart = System.currentTimeMillis
    var numRead = 0
    scala.io.Source.fromFile(filename).getLines.foreach(line => {
      val (ep, rel, cellVal) = entitiesAndRelFromLine(line, colsPerEnt)
      kb.set(ep, rel, cellVal)

      numRead += 1
      if (numRead % 100000 == 0) {
        val tRead = numRead / (System.currentTimeMillis - tReadStart).toDouble
        println(f"cells read per millisecond: $tRead%.4f")
        println(f"Last row: (${ep})")
        println(f"Last column: (${rel})")
        println(f"Last cell value: $cellVal%.4f")
      }
    })
    kb
  }


  def fromRowColumnTsvMinFreq(filename:String, minFreq: Int) : StringStringKBMatrix = {
    val epCounter = new util.HashMap[Int, Int](10000000)
    val memHack = true
    val relCounter = if (memHack) {
      epCounter
    } else {
      new util.HashMap[Int, Int](10000000)
    }

    var numRead = 0

    scala.io.Source.fromFile(filename).getLines.foreach(line => {
      val parts = line.split("\t")
      if (parts.length != 3 && parts.length != 2) {
        println("[WARNING] Line specifying matrix cell needs to specify row, column and (optionally) count for each line.")
      } else {
        val ep: String = parts(0)
        val rel: String = parts(1)

        numRead += 1
        if (numRead % 100000 == 0) {
          print(".")
        }

        val epc = if (epCounter.containsKey(ep.hashCode)) epCounter.get(ep.hashCode) else 0
        val relc = if (relCounter.containsKey(rel.hashCode)) relCounter.get(rel.hashCode) else 0
        epCounter.put(ep.hashCode, epc + 1)
        relCounter.put(rel.hashCode, epc + 1)
      }
    })
    println()

    val kb = new StringStringKBMatrix()

    val tReadStart = System.currentTimeMillis
    numRead = 0
    scala.io.Source.fromFile(filename).getLines.foreach(line => {
      val parts = line.split("\t")
      if (parts.length != 3 && parts.length != 2) {
        println("[WARNING] Line specifying matrix cell needs to specify row, column and (optionally) count for each line.")
      } else {
        val ep: String = parts(0)
        val rel: String = parts(1)
        val cellVal: Double = if (parts.length == 3) parts(2).toDouble else 1
        if (epCounter.get(ep.hashCode) >= minFreq &&
          relCounter.get(rel.hashCode) >= minFreq) {
          kb.set(ep, rel, cellVal)
          numRead += 1
          if (numRead % 100000 == 0) {
            val tRead = numRead / (System.currentTimeMillis - tReadStart).toDouble
            println(f"cells read per millisecond: $tRead%.4f")
            println(f"Last row: (${ep}s)")
            println(f"Last column: (${rel}s)")
            println(f"Last cell value: $cellVal%.4f")
          }
        }
      }
    })
    kb
  }

  def fromRowColumnTsv(filename:String) : StringStringKBMatrix = {
    val kb = new StringStringKBMatrix()

    val tReadStart = System.currentTimeMillis
    var numRead = 0
    scala.io.Source.fromFile(filename).getLines.foreach(line => {

      val parts = line.split("\t")
      if (parts.length != 3 && parts.length != 2) {
        throw new IllegalArgumentException("Line specifying matrix cell needs to specify row, column and (optionally) count for each line.")
      }


      val ep : String = parts(0)
      val rel : String = parts(1)
      val cellVal : Double = if (parts.length == 3) parts(2).toDouble else 1

      kb.set(ep, rel, cellVal)

      numRead += 1
      if (numRead % 100000 == 0) {
        val tRead = numRead / (System.currentTimeMillis - tReadStart).toDouble
        println(f"cells read per millisecond: $tRead%.4f")
        println(f"Last row: (${ep}s)")
        println(f"Last column: (${rel}s)")
        println(f"Last cell value: $cellVal%.4f")
      }
    })
    kb
  }
}




object EntityPairStringKBMatrix {

  private def entitiesAndRelFromLine(line: String, colsPerEnt:Int): (EntityPair, String, Double) = {
    val parts = line.split("\t")
    if (parts.length < 2 * colsPerEnt + 2) {
      throw new IllegalArgumentException("Line specifying matrix cell needs columns for 2 entities, relation, and count.")
    }
    val e1 : String = parts.slice(0, colsPerEnt).mkString("\t")
    val e2 : String = parts.slice(colsPerEnt, 2 * colsPerEnt).mkString("\t")
    val rel : String = parts.slice(2 * colsPerEnt, parts.length - 1).mkString("\t")
    val cellVal : Double = parts(parts.length - 1).toDouble
    (EntityPair(e1, e2), rel, cellVal)
  }

  // Loads a matrix from a tab-separated file
  def fromTsv(filename:String, colsPerEnt:Int = 2) : EntityPairStringKBMatrix = {
    val kb = new EntityPairStringKBMatrix()
    val tReadStart = System.currentTimeMillis
    var numRead = 0
    scala.io.Source.fromFile(filename).getLines.foreach(line => {
      val (ep, rel, cellVal) = entitiesAndRelFromLine(line, colsPerEnt)
      kb.set(ep, rel, cellVal)

      numRead += 1
      if (numRead % 100000 == 0) {
        val tRead = numRead / (System.currentTimeMillis - tReadStart).toDouble
        println(f"cells read per millisecond: $tRead%.4f")
        println(f"Last row: (${ep.e1}s, ${ep.e2}s)")
        println(f"Last column: (${rel}s)")
        println(f"Last cell value: $cellVal%.4f")
      }
    })
    kb
  }


  def fromTsvMongoBacked(mongoDb: DB, filename:String, colsPerEnt:Int = 2) : EntityPairStringKBMatrix = {

    val rowMap = new EntityPairMongoMap(mongoDb = mongoDb, collectionPrefix = MongoWritable.ENTITY_ROW_MAP_PREFIX)
    val colMap = new StringMongoMap(mongoDb = mongoDb, collectionPrefix = MongoWritable.ENTITY_COL_MAP_PREFIX)

    val kb = new EntityPairStringKBMatrix(__rowMap = rowMap, __colMap = colMap)

    val tReadStart = System.currentTimeMillis
    var numRead = 0
    scala.io.Source.fromFile(filename).getLines.foreach(line => {
      val (ep, rel, cellVal) = entitiesAndRelFromLine(line, colsPerEnt)
      kb.set(ep, rel, cellVal)

      numRead += 1
      if (numRead % 100000 == 0) {
        val tRead = numRead / (System.currentTimeMillis - tReadStart).toDouble
        println(f"cells read per millisecond: $tRead%.4f")
        println(f"Last row: (${ep.e1}s, ${ep.e2}s)")
        println(f"Last column: (${rel}s)")
        println(f"Last cell value: $cellVal%.4f")
      }
    })
    kb
  }
}
