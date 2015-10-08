package cc.factorie.epistemodb

import cc.factorie.util.{DoubleAccumulator, Threading}
import scala.util.Random
import scala.collection._
import cc.factorie.la.{Tensor, WeightsMapAccumulator, DenseTensor1}
import cc.factorie.optimize.{Example, AdaGradRDA}
import cc.factorie.app.nlp.embeddings.LiteHogwildTrainer
import cc.factorie.model.{Weights, Weights1}

/**
 * Created by beroth on 2/19/15.
 */
abstract class BprTrainer {
  def stepsize: Double
  def dim: Int
  def matrix: CoocMatrix
  def model: MatrixModel
  def random: Random

  def updateBprCellsWithinColumn(rowIndexTrue: Int, rowIndexFalse: Int, colIndex: Int): Double

  def updateBprOnRows(rowTrue: UniversalSchemaModel.Row, rowFalse: UniversalSchemaModel.Row): Double = {
    val rowIdxTrue = rowTrue._1
    val rowIdxFalse = rowFalse._1
    val colIndicesTrueRow = rowTrue._2.keys
    val colIndicesFalseRow = rowFalse._2.keySet
    // for only updating those where ranking is incorrect, check: model.score(rowTrueIdx, ci) < model.score(rowFalseIdx, ci)
    val data: Iterable[Int] = colIndicesTrueRow.filter(!colIndicesFalseRow.contains(_))
    //    colIndices1.filter(!colIndices2.contains(_)).flatMap(ci => List((rowTrueIdx, rowFalseIdx, ci)))
    val shuffled: Iterable[Int] = random.shuffle(data)
    val objectives = shuffled.map(ci => updateBprCellsWithinColumn(rowIdxTrue, rowIdxFalse, ci))
    //println("positive indices: " + colIndices1.length + "\n updates: " + data.length + "\n objective: " + objectives.sum)
    objectives.sum
  }

  // This is unshuffled.
  def getRowRowColTuples(rowTrue: UniversalSchemaModel.Row, rowFalse: UniversalSchemaModel.Row):
    Iterable[(Int, Int, Int, Boolean)] = {
    val rowIdxTrue = rowTrue._1
    val rowIdxFalse = rowFalse._1
    val colIndicesTrueRow = rowTrue._2.keys
    val colIndicesFalseRow = rowFalse._2.keySet
    val data: Iterable[Int] = colIndicesTrueRow.filter(!colIndicesFalseRow.contains(_))
    data.map(col => (rowIdxTrue, rowIdxFalse, col, true))
  }

  def train(numIters: Int): IndexedSeq[Double] = {
//    val pool = Threading.newFixedThreadPool(1)
// TODO: parallelize
    val pool = Threading.newFixedThreadPool(Runtime.getRuntime.availableProcessors())
    var printNext = 1;
    val objSeq = for (t <- 0 until numIters) yield {
      var objective = 0.0
      // random order, for bpr
      //println("shuffling ...")
      // : Array[mutable.Iterable[UniversalSchemaModel.Row]]
      val batchedRows = random.shuffle(matrix.rowToColAndVal).toSeq.grouped(1000).toArray
      //println("... done.")

      Threading.parForeach(batchedRows, pool)(rowBatch => {
        // Take a sliding window of two rows, and do bpr update.
        val thisObjective = rowBatch.sliding(2).foldLeft(0.0)((obj, rowPair) =>
          obj + updateBprOnRows(rowPair(0), rowPair(1))
        )
        // For non-bpr this would be:
        //val thisObjective = exs.foldLeft(0.0)((b, a) => b + updateOnRow(a._1, a._2, stepsize))
        objective += thisObjective
      })
      if (t == printNext || t == 0 || t == numIters - 1) {
        println("finished iter " + t + " objective = " + objective / matrix.rowToColAndVal.size)
        printNext *= 2
      }
      objective
    }
    pool.shutdown()
    objSeq
  }
}

abstract class RowAndColumnBprTrainer extends BprTrainer {

  val extraShuffle = true
  def updateBprCellsWithinRow(rowIndex: Int, colIndexTrue: Int, colIndexFalse: Int): Double

  // This is unshuffled.
  def getRowColColTuples(row: UniversalSchemaModel.Row):
    Iterable[(Int, Int, Int, Boolean)] = {
    val rowIdx = row._1
    val trueColIndices = row._2.keys
    val trueColIndexSet = row._2.keySet

    // Get random column negative indices - initially generate 2x more than needed, then filter out those colliding
    // with positive ones.
    val falseColIndices = Range(0, trueColIndices.size * 2).
      map(x => random.nextInt(matrix.numCols())).
      filterNot(trueColIndexSet.contains(_))

    trueColIndices.zip(falseColIndices).map(tfc => (rowIdx, tfc._1, tfc._2, false))
  }

  def updateBprOnRow(row: UniversalSchemaModel.Row): Double = {
    val rowIdx = row._1
    val trueColIndices = random.shuffle(row._2.keys)
    val trueColIndexSet = row._2.keySet

    // Get random column negative indices - initially generate 2x more than needed, then filter out those colliding
    // with positive ones.
    val falseColIndices = Range(0, trueColIndices.size * 2).
      map(x => random.nextInt(matrix.numCols())).
      filterNot(trueColIndexSet.contains(_))

    val trueFalseCols = trueColIndices.zip(falseColIndices)
    val objectives = trueFalseCols.map(tfc => updateBprCellsWithinRow(rowIdx, tfc._1, tfc._2))
    objectives.sum
  }
  
  override def train(numIters: Int): IndexedSeq[Double] = {
    //    val pool = Threading.newFixedThreadPool(1)
    // TODO: parallelize
    val pool = Threading.newFixedThreadPool(Runtime.getRuntime.availableProcessors())
    var printNext = 1;
    val objSeq = for (t <- 0 until numIters) yield {
      var objective = 0.0
      // random order, for bpr
      //println("shuffling ...")
      // : Array[mutable.Iterable[UniversalSchemaModel.Row]]
      val batchedRows = random.shuffle(matrix.rowToColAndVal).toSeq.grouped(1000).toArray
      //println("... done.")



      Threading.parForeach(batchedRows, pool)(rowBatch => {

        if (extraShuffle) {
          // collect all tuples.
          val tuples: Iterable[(Int, Int, Int, Boolean)] =
            (
            rowBatch.sliding(2).flatMap(rowPair => getRowRowColTuples(rowPair(0), rowPair(1))) ++
            rowBatch.flatMap(row => getRowColColTuples(row))
              ).toIterable


          // shuffle and do updates on single tuple basis, depending on wether the tuple is within the same colum (true)
          // or not (false)
          val thisObj = random.shuffle(tuples).take(tuples.size/2).
            foldLeft(0.0)((obj, tuple) =>
            if (tuple._4) {
              obj + updateBprCellsWithinColumn(tuple._1, tuple._2, tuple._3)
            } else {
              obj + updateBprCellsWithinRow(tuple._1, tuple._2, tuple._3)
            }
          )
          objective += thisObj

        } else {
          // Do within-row update for current row
          val thisRowObjective = rowBatch.foldLeft(0.0)((obj, row) =>
            obj + updateBprOnRow(row)
          )

          // Take a sliding window of two rows, and do column-wise bpr update for those two rows.
          val thisColObjective = rowBatch.sliding(2).foldLeft(0.0)((obj, rowPair) =>
            obj + updateBprOnRows(rowPair(0), rowPair(1))
          )

          // For non-bpr this would be:
          //val thisObjective = exs.foldLeft(0.0)((b, a) => b + updateOnRow(a._1, a._2, stepsize))
          objective += thisColObjective + thisRowObjective
        }
      })
      if (t == printNext || t == 0 || t == numIters - 1) {
        println("finished iter " + t + " objective = " + objective / matrix.rowToColAndVal.size)
        printNext *= 2
      }
      objective
    }
    pool.shutdown()
    objSeq
  }
}


class RuleBprTrainer(val regularizer: Double, val stepsize: Double, val dim: Int,
                     val matrix: CoocMatrix, val model: UniversalSchemaModel, val random: Random,


                     var rules : Seq[(Int, Int)]) extends RowAndColumnBprTrainer {
  val rowRegularizer = regularizer
  val colRegularizer = regularizer
//===
// TODO: put this in extra trait
  override def updateBprCellsWithinColumn(rowIndexTrue: Int, rowIndexFalse: Int, colIndex: Int): Double = {
    val scoreTrueCell = model.score(rowIndexTrue, colIndex)
    val scoreFalseCell = model.score(rowIndexFalse, colIndex)
    val theta = scoreTrueCell - scoreFalseCell
    val prob = UniversalSchemaModel.calculateProb(theta)

    // TODO: include reguarizer into objective logged to output:
    // - sq(rowVectors) * model.rowRegularizer / 2
    // - sq(colVectors) * model.colRegularizer
    var thisObjective = math.log(prob)

    val step = stepsize * (1 - prob)
    val colVec = model.colVectors(colIndex).copy

    model.colVectors(colIndex).*=((1 - stepsize * colRegularizer))
    model.colVectors(colIndex).+=(model.rowVectors(rowIndexTrue) - model.rowVectors(rowIndexFalse), step)

    model.rowVectors(rowIndexTrue).*=((1 - stepsize * rowRegularizer))
    model.rowVectors(rowIndexTrue).+=(colVec, step)

    model.rowVectors(rowIndexFalse).*=((1 - stepsize * rowRegularizer))
    model.rowVectors(rowIndexFalse).+=(colVec, -step)

    thisObjective
  }


  override def updateBprCellsWithinRow(rowIndex: Int, colIndexTrue: Int, colIndexFalse: Int): Double = {
    val scoreTrueCell = model.score(rowIndex, colIndexTrue)
    val scoreFalseCell = model.score(rowIndex, colIndexFalse)

    val theta = scoreTrueCell - scoreFalseCell
    val prob = UniversalSchemaModel.calculateProb(theta)

    // TODO: include reguarizer into objective logged to output:
    // - sq(rowVectors) * model.rowRegularizer / 2
    // - sq(colVectors) * model.colRegularizer
    var thisObjective = math.log(prob)

    val step = stepsize * (1 - prob)

    val rowVec = model.rowVectors(rowIndex).copy

    model.rowVectors(rowIndex).*=((1 - stepsize * rowRegularizer))
    model.rowVectors(rowIndex).+=(model.colVectors(colIndexTrue) - model.colVectors(colIndexFalse), step)

    model.colVectors(colIndexTrue).*=((1 - stepsize * colRegularizer))
    model.colVectors(colIndexTrue).+=(rowVec, step)

    model.colVectors(colIndexFalse).*=((1 - stepsize * colRegularizer))
    model.colVectors(colIndexFalse).+=(rowVec, -step)

    thisObjective
  }
  // TODO: put the above in extra trait
  //===


  def getRowColColTupleFromRules(row: UniversalSchemaModel.Row): Option[(Int, Int, Int, Boolean)] =
    if (rules.isEmpty || random.nextDouble() > 0.2) {
      None
    } else {
      val rowIdx = row._1
      val rule = rules(random.nextInt(rules.size))
      val trueColIdx = rule._2
      val falseColIdx = rule._1
      Some((rowIdx, trueColIdx, falseColIdx, false))
    }

  override def train(numIters: Int): IndexedSeq[Double] = {
    //    val pool = Threading.newFixedThreadPool(1)
    // TODO: parallelize
    val pool = Threading.newFixedThreadPool(Runtime.getRuntime.availableProcessors())
    var printNext = 1;
    val objSeq = for (t <- 0 until numIters) yield {
      var objective = 0.0
      // random order, for bpr
      //println("shuffling ...")
      // : Array[mutable.Iterable[UniversalSchemaModel.Row]]
      val batchedRows = random.shuffle(matrix.rowToColAndVal).toSeq.grouped(1000).toArray
      //println("... done.")

      Threading.parForeach(batchedRows, pool)(rowBatch => {
        // collect all tuples.



        val tuples: Iterable[(Int, Int, Int, Boolean)] =
          (
            rowBatch.sliding(2).flatMap(rowPair => getRowRowColTuples(rowPair(0), rowPair(1))) ++
              rowBatch.flatMap(row => getRowColColTuples(row)) ++
              rowBatch.map(row => getRowColColTupleFromRules(row)).flatten

            ).toIterable


        // shuffle and do updates on single tuple basis, depending on wether the tuple is within the same colum (true)
        // or not (false)
        val thisObj = random.shuffle(tuples).take(tuples.size/2).
          foldLeft(0.0)((obj, tuple) =>
          if (tuple._4) {
            obj + updateBprCellsWithinColumn(tuple._1, tuple._2, tuple._3)
          } else {
            obj + updateBprCellsWithinRow(tuple._1, tuple._2, tuple._3)
          }
          )
        objective += thisObj
      })
      if (t == printNext || t == 0 || t == numIters - 1) {
        println("finished iter " + t + " objective = " + objective / matrix.rowToColAndVal.size)
        printNext *= 2
      }
      objective
    }
    pool.shutdown()
    objSeq
  }
}

class TransEExample(val posVecE1: Weights, val posVecE2: Weights, val negVecE1: Weights, val negVecE2: Weights,
                    val  colVec: Weights, val posGrad: Tensor, val negGrad: Tensor) extends Example {

  def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator): Unit = {
    gradient.accumulate(posVecE1, posGrad, 1.0)
    gradient.accumulate(posVecE2, posGrad, -1.0)
    gradient.accumulate(negVecE1, negGrad, -1.0)
    gradient.accumulate(negVecE2, negGrad, 1.0)
    gradient.accumulate(colVec, posGrad - negGrad, 1.0)
  }
}

class TransETrainer(val regularizer: Double, val stepsize: Double, val margin : Double, val dim: Int,
                                           val matrix: CoocMatrix, val model: TransEModel, val random: Random) extends
BprTrainer {
  val entityRegularizer = regularizer
  val colRegularizer = regularizer

  val optimizer = new AdaGradRDA(delta = 0.01 , rate = stepsize, l2 = regularizer)
  val trainer = new LiteHogwildTrainer(weightsSet = model.parameters, optimizer = optimizer, maxIterations = Int.MaxValue)

  optimizer.initializeWeights(model.parameters)

  override def updateBprCellsWithinColumn(rowIndexTrue: Int, rowIndexFalse: Int /*not used*/, colIndex: Int): Double = {

    val (posE1, posE2) = this.model.rowToEnts(rowIndexTrue)

    val takeE1 = random.nextBoolean()
    val negE = random.nextInt(model.numEnts)

    val (negE1, negE2) = if (takeE1) {
      (negE, posE2)
    } else {
      (posE1, negE)
    }

    val posGrad = model.gradient(posE1, posE2, colIndex)
    val negGrad = model.gradient(negE1, negE2, colIndex)

    val obj = margin + posGrad.twoNorm - negGrad.twoNorm

    if (obj > 0.0) {
      val posVecE1 = model.entityVectors(posE1)
      val posVecE2 = model.entityVectors(posE2)
      val negVecE1 = model.entityVectors(negE1)
      val negVecE2 = model.entityVectors(negE2)
      val colVec = model.colVectors(colIndex)

      val ex = new TransEExample(posVecE1, posVecE2, negVecE1, negVecE2,  colVec, posGrad, negGrad)
      trainer.processExample(ex)

    }
    -obj
  }
}

class RegularizedBprUniversalSchemaTrainer(val regularizer: Double, val stepsize: Double, val dim: Int,
                                            val matrix: CoocMatrix, val model: UniversalSchemaModel, val random: Random) extends
      BprTrainer {
  val rowRegularizer = regularizer
  val colRegularizer = regularizer

  override def updateBprCellsWithinColumn(rowIndexTrue: Int, rowIndexFalse: Int, colIndex: Int): Double = {
    val scoreTrueCell = model.score(rowIndexTrue, colIndex)
    val scoreFalseCell = model.score(rowIndexFalse, colIndex)
    val theta = scoreTrueCell - scoreFalseCell
    val prob = UniversalSchemaModel.calculateProb(theta)

    // TODO: include reguarizer into objective logged to output:
    // - sq(rowVectors) * model.rowRegularizer / 2
    // - sq(colVectors) * model.colRegularizer
    var thisObjective = math.log(prob)

    val step = stepsize * (1 - prob)
    val colVec = model.colVectors(colIndex).copy

    model.colVectors(colIndex).*=((1 - stepsize * colRegularizer))
    model.colVectors(colIndex).+=(model.rowVectors(rowIndexTrue) - model.rowVectors(rowIndexFalse), step)

    model.rowVectors(rowIndexTrue).*=((1 - stepsize * rowRegularizer))
    model.rowVectors(rowIndexTrue).+=(colVec, step)

    model.rowVectors(rowIndexFalse).*=((1 - stepsize * rowRegularizer))
    model.rowVectors(rowIndexFalse).+=(colVec, -step)

    thisObjective
  }
}


class RegularizedBprRowAndColumnUniversalSchemaTrainer(val regularizer: Double, val stepsize: Double, val dim: Int,
                                           val matrix: CoocMatrix, val model: UniversalSchemaModel, val random: Random) extends
RowAndColumnBprTrainer {
  val rowRegularizer = regularizer
  val colRegularizer = regularizer

  override def updateBprCellsWithinColumn(rowIndexTrue: Int, rowIndexFalse: Int, colIndex: Int): Double = {
    val scoreTrueCell = model.score(rowIndexTrue, colIndex)
    val scoreFalseCell = model.score(rowIndexFalse, colIndex)
    val theta = scoreTrueCell - scoreFalseCell
    val prob = UniversalSchemaModel.calculateProb(theta)

    // TODO: include reguarizer into objective logged to output:
    // - sq(rowVectors) * model.rowRegularizer / 2
    // - sq(colVectors) * model.colRegularizer
    var thisObjective = math.log(prob)

    val step = stepsize * (1 - prob)
    val colVec = model.colVectors(colIndex).copy

    model.colVectors(colIndex).*=((1 - stepsize * colRegularizer))
    model.colVectors(colIndex).+=(model.rowVectors(rowIndexTrue) - model.rowVectors(rowIndexFalse), step)

    model.rowVectors(rowIndexTrue).*=((1 - stepsize * rowRegularizer))
    model.rowVectors(rowIndexTrue).+=(colVec, step)

    model.rowVectors(rowIndexFalse).*=((1 - stepsize * rowRegularizer))
    model.rowVectors(rowIndexFalse).+=(colVec, -step)

    thisObjective
  }


  override def updateBprCellsWithinRow(rowIndex: Int, colIndexTrue: Int, colIndexFalse: Int): Double = {
    val scoreTrueCell = model.score(rowIndex, colIndexTrue)
    val scoreFalseCell = model.score(rowIndex, colIndexFalse)

    val theta = scoreTrueCell - scoreFalseCell
    val prob = UniversalSchemaModel.calculateProb(theta)

    // TODO: include reguarizer into objective logged to output:
    // - sq(rowVectors) * model.rowRegularizer / 2
    // - sq(colVectors) * model.colRegularizer
    var thisObjective = math.log(prob)

    val step = stepsize * (1 - prob)

    val rowVec = model.rowVectors(rowIndex).copy

    model.rowVectors(rowIndex).*=((1 - stepsize * rowRegularizer))
    model.rowVectors(rowIndex).+=(model.colVectors(colIndexTrue) - model.colVectors(colIndexFalse), step)

    model.colVectors(colIndexTrue).*=((1 - stepsize * colRegularizer))
    model.colVectors(colIndexTrue).+=(rowVec, step)

    model.colVectors(colIndexFalse).*=((1 - stepsize * colRegularizer))
    model.colVectors(colIndexFalse).+=(rowVec, -step)

    thisObjective
  }
}

class NormConstrainedBprUniversalSchemaTrainer(val maxNorm: Double, val stepsize: Double, val dim: Int,
                                           val matrix: CoocMatrix, val model: UniversalSchemaModel, val random: Random) extends
    BprTrainer {

  val rowNormConstraint = maxNorm
  val colNormConstraint = maxNorm

  def constrain(vector: DenseTensor1, maxNorm: Double) {
    val norm = vector.twoNorm
    if (norm > maxNorm) {
      vector *= (maxNorm / norm)
    }
  }

  override def updateBprCellsWithinColumn(rowIndexTrue: Int, rowIndexFalse: Int, colIndex: Int): Double = {
    val scoreTrueCell = model.score(rowIndexTrue, colIndex)
    val scoreFalseCell = model.score(rowIndexFalse, colIndex)
    val theta = scoreTrueCell - scoreFalseCell
    val prob = UniversalSchemaModel.calculateProb(theta)

    var thisObjective = math.log(prob)

    val step = stepsize * (1 - prob)
    val colVec = model.colVectors(colIndex).copy

    model.colVectors(colIndex).+=(model.rowVectors(rowIndexTrue) - model.rowVectors(rowIndexFalse), step)
    constrain(model.colVectors(colIndex), colNormConstraint)

    model.rowVectors(rowIndexTrue).+=(colVec, step)
    constrain(model.rowVectors(rowIndexTrue), rowNormConstraint)

    model.rowVectors(rowIndexFalse).+=(colVec, -step)
    constrain(model.rowVectors(rowIndexFalse), rowNormConstraint)

    thisObjective
  }
}



