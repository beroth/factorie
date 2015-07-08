package cc.factorie.epistemodb.activelearning

import cc.factorie.epistemodb.{Evaluator, RegularizedBprUniversalSchemaTrainer, UniversalSchemaModel, CoocMatrix}
import scala.util.Random
import scala.Some

class ModelEvaluator(val name: String) {
  var results = List[Double]()
  var rulesApplied = List[Double]()
  var cellsFilledCorrect = List[Double]()
  var cellsFilledIncorrect = List[Double]()

  var acceptedRules = 0.0
  var filledCellsIfHolds = 0.0
  var filledCellsIfNotHolds = 0.0

  var model: UniversalSchemaModel = null
  var trainer: RegularizedBprUniversalSchemaTrainer = null
  var mTrain: CoocMatrix = null

  def initModel(trainMatrix: CoocMatrix, dim: Int = 10, regularizer: Double = 0.01, stepsize: Double = 0.1, seed: Int = 0): Unit = {
    mTrain = trainMatrix.copy()
    model = UniversalSchemaModel.randomModel(mTrain.numRows(), mTrain.numCols(), dim, new Random(seed))
    trainer = new RegularizedBprUniversalSchemaTrainer(regularizer, stepsize, dim, mTrain, model, new Random(seed))
  }

  def meanAveragePrecision(testMatrix: CoocMatrix): Double = {
    val inititalResultGoldAnno = model.similaritiesAndLabels(mTrain, testMatrix)
    Evaluator.meanAveragePrecision(inititalResultGoldAnno)
  }

  def annotateHardConstraints(antecedents: Seq[Int], targetIdx: Int, numTopics: Int): Unit = {
    for (antecedentIdx <- antecedents) {
      val relationHolds = ((antecedentIdx % numTopics) == (targetIdx % numTopics))
      if (relationHolds) {
        acceptedRules += 1
        for(row <- mTrain.colToRows.get(antecedentIdx).get) {
          // Get all nnz rows for antecedent
          val antecedentVal = mTrain.get(row, antecedentIdx)
          if (antecedentVal == 1) {
            if (antecedentVal > mTrain.get(row, targetIdx)) {
              mTrain.set(row, targetIdx, antecedentVal)
              if (row % numTopics == antecedentIdx % numTopics) {
                filledCellsIfHolds += 1
              } else {
                filledCellsIfNotHolds += 1
              }
            }
          }
        }
      }
    }
  }

  def evaluateAndAggregate(testMatrix: CoocMatrix) : Unit = {
    results = meanAveragePrecision(testMatrix) :: results
    rulesApplied = acceptedRules :: rulesApplied
    cellsFilledCorrect = filledCellsIfHolds :: cellsFilledCorrect
    cellsFilledIncorrect = filledCellsIfNotHolds :: cellsFilledIncorrect
    acceptedRules = 0.0
    filledCellsIfHolds = 0.0
    filledCellsIfNotHolds = 0.0
  }
}

/**
 * Created by beroth on 6/25/15.
 */
object SyntheticActiveLearningExperiments {

  def stddev(xs: List[Double], avg: Double): Double = xs match {
    case Nil => 0.0
    case ys => math.sqrt((0.0 /: ys) {
      (a,e) => a + math.pow(e - avg, 2.0)
    } / (xs.size))
  }


  def meanAndStandardError(l : List[Double]) : (Double, Double) = {
    val mean = l.reduceLeft(_ + _) / l.size
    val sdev : Double = stddev(l, mean)
    val stderr = sdev / math.sqrt(l.size)
    (mean, stderr)
  }

  def main(args: Array[String]) : Unit = {

    val numRows = 1000
    val numCols = 1000
    val nnz = 5000

    val numTopics = 100
    val dim = 10
    val noise1 = 0.1

    val stepsize = 0.1
    val regularizer = 0.01

    val numDevNNZ = 0
    val numTestNNZ = 0//150

    // Test matrix is constructed following underlying pattern.
    val mTest = new CoocMatrix(numRows, numCols)
    val testCols = Set(0,1,2,3,4,5,6,7,8,9)
    for (col <- testCols) {
      for (row <- Range(0, numRows)) {
        if (row % numTopics == col % numTopics) {
          mTest.set(row, col, 1.0)
        }
      }
    }

    val evaluatorGold = new ModelEvaluator("Gold Annotation")
    val evaluatorAnno = new ModelEvaluator("Rule-based Annotation")
    val evaluatorRandom = new ModelEvaluator("Random Annotation")
    val evaluatorNoAnno = new ModelEvaluator("No Annotation")
    val evaluatorSimilarity = new ModelEvaluator("Similarity-based Annotation")
    val evaluatorChange = new ModelEvaluator("Change-based Annotation")

    val evaluators = Seq(evaluatorGold, evaluatorAnno, evaluatorRandom, evaluatorNoAnno, evaluatorSimilarity, evaluatorChange)

    var numRulesProposed = 10

    for (seed <- 0 until 10) {
      val random = new Random(seed)
      val m = CoocMatrix.randomOneZeroMatrix(numRows, numCols, nnz, random, numTopics, noise1) //.prune(1,1)._1

      println("nnz: " + m.nnz())

      val (mTrain,mDev,mTestUnused) = m.randomTestSplit(numDevNNZ, numTestNNZ, None, Some(testCols), random)

      val seedForModels = random.nextInt()

      evaluators.foreach(ev => {
        ev.initModel(mTrain, seed = seedForModels)
        ev.trainer.train(10)
        println("\nInitial "+ ev.name +": " + ev.meanAveragePrecision(mTest) + "\n")
      })

      for (targetIdx <- testCols) {
        println("current target column: " + targetIdx)

        for(row <- Range(0, evaluatorGold.mTrain.numRows())) {
          if (row % numTopics == targetIdx % numTopics) {
            evaluatorGold.mTrain.set(row, targetIdx, 1.0)
          }
        }

        val annoAntecedents = evaluatorAnno.model.columnToExpectedRankingGain(evaluatorAnno.mTrain, targetIdx, 2).
          toSeq.sortBy(-_._2).slice(0,numRulesProposed)
        evaluatorAnno.annotateHardConstraints(annoAntecedents.map(_._1), targetIdx, numTopics)

        val similarAntecedents = evaluatorSimilarity.model.columnToSimilarity(evaluatorSimilarity.mTrain, targetIdx, 2).
          toSeq.sortBy(-_._2).slice(0,numRulesProposed)
        evaluatorSimilarity.annotateHardConstraints(similarAntecedents.map(_._1), targetIdx, numTopics)

        val changeAntecedents = evaluatorChange.model.columnToChange(evaluatorChange.mTrain, targetIdx, 2).
          toSeq.sortBy(-_._2).slice(0,numRulesProposed)
        evaluatorChange.annotateHardConstraints(changeAntecedents.map(_._1), targetIdx, numTopics)

        val randomAntecedents = random.shuffle(
          evaluatorRandom.model.columnToExpectedRankingGain(evaluatorRandom.mTrain, targetIdx, 2).toSeq
        ).slice(0,numRulesProposed)
        evaluatorRandom.annotateHardConstraints(randomAntecedents.map(_._1), targetIdx, numTopics)

        println("===")
        println("best antecedents:")
        println("ERI: " + annoAntecedents)
        println("change: " + changeAntecedents)
        println("similarity: " + similarAntecedents)
      }

      evaluators.foreach(ev => {
        println(ev.name)
        println("Antecedent cells following pattern: " + ev.filledCellsIfHolds)
        println("Antecedent cells not following pattern: " + ev.filledCellsIfNotHolds)
        println("===")
      })

      evaluators.foreach(ev => {
        println("training: " + ev.name)
        ev.trainer.train(10)
        ev.evaluateAndAggregate(mTest)
        println("last map: " + ev.results(0))
        println("===")
      })
    }

    println("Mean and standard error:")
    println("===")
    evaluators.foreach(ev => {
      println(ev.name)
      println("map: " + meanAndStandardError(ev.results))
      println("rules applied: " + meanAndStandardError(ev.rulesApplied))
      println("Cells correctly filled: " + meanAndStandardError(ev.cellsFilledCorrect))
      println("Cells incorrectly filled: " + meanAndStandardError(ev.cellsFilledIncorrect))
      println("===")
    })
  }
}
