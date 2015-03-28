package cc.factorie.epistemodb

import com.mongodb._
import scala.Some

/**
 * Created by beroth on 2/6/15.
 */
/**
 * Holds a knowledge-base with an underlying matrix.
 * I.e. additionally to matrix information, it also stores information about entities, relations etc.
 */

class EntityRelationKBMatrix(val matrix:CoocMatrix = new CoocMatrix(0,0),
               val __rowMap: MatrixIndexMap[EntityPair] with MongoWritable = new EntityPairMemoryMap(collectionPrefix = MongoWritable.ENTITY_ROW_MAP_PREFIX),
               val __colMap: MatrixIndexMap[String] with MongoWritable = new StringMemoryIndexMap(collectionPrefix = MongoWritable.ENTITY_COL_MAP_PREFIX)
                              ) extends KBMatrix[EntityRelationKBMatrix, EntityPair, String] with MongoWritable {

  def cloneWithNewCells(cells: CoocMatrix): EntityRelationKBMatrix = {
    new EntityRelationKBMatrix(matrix = cells, __rowMap = this.__rowMap, __colMap = this.__colMap)
  }

  def createEmptyMatrix(): EntityRelationKBMatrix = {
    new EntityRelationKBMatrix()
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
        println(f"Last row: (${ep}s)")
        println(f"Last column: (${rel}s)")
        println(f"Last cell value: $cellVal%.4f")
      }
    })
    kb
  }
}




object EntityRelationKBMatrix {

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
  def fromTsv(filename:String, colsPerEnt:Int = 2) : EntityRelationKBMatrix = {
    val kb = new EntityRelationKBMatrix()
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


  def fromTsvMongoBacked(mongoDb: DB, filename:String, colsPerEnt:Int = 2) : EntityRelationKBMatrix = {

    val rowMap = new EntityPairMongoMap(mongoDb = mongoDb, collectionPrefix = MongoWritable.ENTITY_ROW_MAP_PREFIX)
    val colMap = new StringMongoMap(mongoDb = mongoDb, collectionPrefix = MongoWritable.ENTITY_COL_MAP_PREFIX)

    val kb = new EntityRelationKBMatrix(__rowMap = rowMap, __colMap = colMap)

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
