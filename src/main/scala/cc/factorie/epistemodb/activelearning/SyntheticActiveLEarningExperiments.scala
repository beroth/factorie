package cc.factorie.epistemodb.activelearning

import cc.factorie.epistemodb._
import scala.util.Random
import scala.Some
import scala.Some

class ModelEvaluator(val name: String, val ruleBased: Boolean = false) {
  var results = List[Double]()
  var resultsGAP = List[Double]()
  var rulesApplied = List[Double]()
  var cellsFilledCorrect = List[Double]()
  var cellsFilledIncorrect = List[Double]()

  var acceptedRules = 0.0
  var filledCellsIfHolds = 0.0
  var filledCellsIfNotHolds = 0.0

  var model: UniversalSchemaModel = null
  var trainer: BprTrainer = null// RegularizedBprUniversalSchemaTrainer = null
  var mTrain: CoocMatrix = null

  def initModel(trainMatrix: CoocMatrix, dim: Int = 10, regularizer: Double = 0.01, stepsize: Double = 0.1, seed: Int = 0): Unit = {
    mTrain = trainMatrix.copy()
    model = UniversalSchemaModel.randomModel(mTrain.numRows(), mTrain.numCols(), dim, new Random(seed))
    trainer = if (ruleBased) {
      new RuleBprTrainer(regularizer, stepsize, dim, mTrain, model, new Random(seed), Seq[(Int, Int)]() )
      //new RegularizedBprRowAndColumnUniversalSchemaTrainer(regularizer, stepsize, dim, mTrain, model, new Random(seed))
    } else {
      new RegularizedBprUniversalSchemaTrainer(regularizer, stepsize, dim, mTrain, model, new Random(seed))
    }
  }

  def proposeRulesBySimilarity(testCols: Iterable[Int], numRulesPerTarget: Int) : Seq[(Int, Int)] = {
    testCols.flatMap(targetIdx => {
      model.columnToSimilarity(mTrain, targetIdx, 2).toSeq.
        sortBy(-_._2).slice(0,numRulesPerTarget).map(antecedent => (antecedent._1, targetIdx))
    }).toSeq
  }

  def meanAveragePrecision(originalTrainMatrix: CoocMatrix, testMatrix: CoocMatrix): Double = {
    val initialResultGoldAnno = model.similaritiesAndLabels(originalTrainMatrix, testMatrix)
    Evaluator.meanAveragePrecision(initialResultGoldAnno)
  }

  def globalAveragePrecision(originalTrainMatrix: CoocMatrix, testMatrix: CoocMatrix): Double = {
    val initialResultGoldAnno = model.similaritiesAndLabels(originalTrainMatrix, testMatrix)
    Evaluator.globalAveragePrecision(initialResultGoldAnno)
  }

  def judgeRules(antecedentsTargets: Seq[(Int, Int)], numTopics: Int):  Seq[(Int, Int)] = {
    antecedentsTargets.filter(rule => (rule._1 % numTopics) == (rule._2 % numTopics))
  }

  def annotateHardConstraints(antecedentsTargets: Seq[(Int, Int)], numTopics: Int): Unit = {

    for (antecTarget <- antecedentsTargets) {
      val antecedentIdx = antecTarget._1
      val targetIdx = antecTarget._2
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

  def evaluateAndAggregate(originalTrainMatrix: CoocMatrix, testMatrix: CoocMatrix) : Unit = {
    results = meanAveragePrecision(originalTrainMatrix, testMatrix) :: results
    resultsGAP = globalAveragePrecision(originalTrainMatrix, testMatrix) :: resultsGAP
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

    val numIters = 50

    // Test matrix is constructed following underlying pattern.
    val mTest = new CoocMatrix(numRows, numCols)
    val testCols = Set(0,1,2,3,4,5,6,7,8,9)
    for (col <- testCols) {
      for (row <- Range(0, numRows)) {
        if (row % numTopics == col % numTopics) {
          println("set " + row + " - " + col)
          mTest.set(row, col, 1.0)
        }
      }
    }

    val evaluatorGold = new ModelEvaluator("Gold Annotation")
    val evaluatorAnno = new ModelEvaluator("Rule-based Annotation")

    val evaluatorAnnoRuleTraining = new ModelEvaluator("Rule-based Annotation / Rule-based training", true)

    val evaluatorRandom = new ModelEvaluator("Random Annotation")
    val evaluatorNoAnno = new ModelEvaluator("No Annotation")
    val evaluatorNoAnnoRowCol = new ModelEvaluator("No Annotation / Row and Col Training", true)

    val evaluatorSimilarity = new ModelEvaluator("Similarity-based Annotation")
    val evaluatorChange = new ModelEvaluator("Change-based Annotation")

//    val evaluators = Seq(evaluatorGold, evaluatorAnno, evaluatorAnnoRuleTraining, evaluatorRandom, evaluatorNoAnno, evaluatorNoAnnoRowCol, evaluatorSimilarity, evaluatorChange)
    ///Seq(evaluatorGold, evaluatorRandom, evaluatorNoAnno, evaluatorNoAnnoRowCol, evaluatorSimilarity, evaluatorChange).foreach(_.initModel(mTrain, seed = seedForModels))

    val evaluators = Seq(evaluatorAnno, evaluatorAnnoRuleTraining)

    var numRulesProposed = 10

    for (seed <- 0 until 10) {
      val random = new Random(seed)
      val m = CoocMatrix.randomOneZeroMatrix(numRows, numCols, nnz, random, numTopics, noise1) //.prune(1,1)._1

      println("nnz: " + m.nnz())

      val (mTrain,mDev,mTestUnused) = m.randomTestSplit(numDevNNZ, numTestNNZ, None, Some(testCols), random)

      val seedForModels = random.nextInt()

      // TODO
      Seq(evaluatorGold, evaluatorRandom, evaluatorNoAnno, evaluatorNoAnnoRowCol, evaluatorSimilarity, evaluatorChange).foreach(_.initModel(mTrain, seed = seedForModels))

      evaluators.foreach(ev => {
        ev.initModel(mTrain, seed = seedForModels)
        ev.trainer.train(numIters)
        println("\nInitial "+ ev.name +": " + ev.meanAveragePrecision(mTrain, mTest) + "\n")
      })

      for (targetIdx <- testCols) {
        println("current target column: " + targetIdx)


        for(row <- Range(0, numRows)) {
          if (row % numTopics == targetIdx % numTopics) {
            println("set " + row + " - " + targetIdx)
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

      val proposedRules = evaluatorAnnoRuleTraining.proposeRulesBySimilarity(testCols, numRulesProposed)
      evaluatorAnnoRuleTraining.annotateHardConstraints(proposedRules, numTopics)
      evaluatorAnnoRuleTraining.trainer.asInstanceOf[RuleBprTrainer].rules = evaluatorAnnoRuleTraining.judgeRules(proposedRules, numTopics)

      evaluators.foreach(ev => {
        println(ev.name)
        println("Antecedent cells following pattern: " + ev.filledCellsIfHolds)
        println("Antecedent cells not following pattern: " + ev.filledCellsIfNotHolds)
        println("===")
      })

      /*
      evaluators.foreach(ev => {
        println("\nBefore training: "+ ev.name +": " + ev.meanAveragePrecision(mTrain, mTest) + "\n")
      })
*/
      evaluators.foreach(ev => {
        println("training: " + ev.name)
        ev.trainer.train(numIters)
        ev.evaluateAndAggregate(mTrain, mTest)
        println("last map: " + ev.results(0))
        println("===")
      })
    }

    println("Mean and standard error:")
    println("===")
    evaluators.foreach(ev => {
      println(ev.name)
      println("map: " + meanAndStandardError(ev.results))
      println("global ap: " + meanAndStandardError(ev.resultsGAP))
      println("rules applied: " + meanAndStandardError(ev.rulesApplied))
      println("Cells correctly filled: " + meanAndStandardError(ev.cellsFilledCorrect))
      println("Cells incorrectly filled: " + meanAndStandardError(ev.cellsFilledIncorrect))
      println("===")
    })

    println(evaluatorGold.results)
    println(evaluatorAnno.results)
  }
}
