package cc.factorie.epistemodb.activelearning

import cc.factorie.epistemodb.{Evaluator, RegularizedBprUniversalSchemaTrainer, UniversalSchemaModel, CoocMatrix}
import scala.util.Random
import scala.Some

def class ModelEvaluator(val name: String) {
  var results = List[Double]()
  var rulesApplied = List[Double]()
  var cellsFilledCorrect = List[Double]()
  var cellsFilledIncorrect = List[Double]()

  var acceptedRules = 0.0
  var filledCellsIfHolds = 0.0
  var filledCellsIfNotHolds = 0.0

  var model: UniversalSchemaModel
  var trainer: RegularizedBprUniversalSchemaTrainer
  var mTrain: CoocMatrix

  def initModel(trainMatrix: CoocMatrix, dim: Int = 10, regularizer: Int = 0.01, stepsize: Int = 0.1, seed: Int = 0): Unit = {
    mTrain = trainMatrix.copy()
    model = UniversalSchemaModel.randomModel(mTrain.numRows(), mTrain.numCols(), dim, new Random(seed))
    trainer = new RegularizedBprUniversalSchemaTrainer(regularizer, stepsize, dim, mTrain, model, new Random(seed))
  }

  def meanAveragePrecision(testMatrix: CoocMatrix): Double = {
    val inititalResultGoldAnno = modelGoldAnnotation.similaritiesAndLabels(mTrain, testMatrix)
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
              mTrainAnno.set(row, targetIdx, antecedentVal)
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

      /*
      val mTrainGoldAnno = mTrain.copy()

      val modelGoldAnnotation = UniversalSchemaModel.randomModel(numRows, numCols, dim, new Random(seedForModels))
      val trainerGoldAnnotation = new RegularizedBprUniversalSchemaTrainer(regularizer, stepsize, dim, mTrainGoldAnno, modelGoldAnnotation, new Random(seedForModels))
      trainerGoldAnnotation.train(10)
      val inititalResultGoldAnno = modelGoldAnnotation.similaritiesAndLabels(mTrain, mTest)
      println("\nInitial MAP gold annotation: " + Evaluator.meanAveragePrecision(inititalResultGoldAnno) + "\n")
      */

      evaluators.foreach(ev => {
        ev.initModel(mTrain, seed = seedForModels)
        ev.trainer.train(10)
        println("\nInitial "+ ev.name +": " + ev.meanAveragePrecision(mTest) + "\n")
      })

/*
      val mTrainAnno = mTrain.copy()
      val modelAnno = UniversalSchemaModel.randomModel(numRows, numCols, dim, new Random(seedForModels))
      val trainerForAnnotation = new RegularizedBprUniversalSchemaTrainer(regularizer, stepsize, dim, mTrainAnno, modelAnno, new Random(seedForModels))
      trainerForAnnotation.train(10)
      val inititalResultAnno = modelAnno.similaritiesAndLabels(mTrain, mTest)
      println("\nInitial MAP selected rules: " + Evaluator.meanAveragePrecision(inititalResultAnno) + "\n")


      val mTrainSimilarityAnno = mTrain.copy()
      val modelSimilarityAnno = UniversalSchemaModel.randomModel(numRows, numCols, dim, new Random(seedForModels))
      val trainerForSimilarityAnnotation = new RegularizedBprUniversalSchemaTrainer(regularizer, stepsize, dim, mTrainSimilarityAnno, modelSimilarityAnno, new Random(seedForModels))
      trainerForSimilarityAnnotation.train(10)
      val inititalResultSimilarityAnno = modelSimilarityAnno.similaritiesAndLabels(mTrain, mTest)
      println("\nInitial MAP selected rules: " + Evaluator.meanAveragePrecision(inititalResultSimilarityAnno) + "\n")

      val mTrainChangeAnno = mTrain.copy()
      val modelChangeAnno = UniversalSchemaModel.randomModel(numRows, numCols, dim, new Random(seedForModels))
      val trainerForChangeAnnotation = new RegularizedBprUniversalSchemaTrainer(regularizer, stepsize, dim, mTrainChangeAnno, modelChangeAnno, new Random(seedForModels))
      trainerForChangeAnnotation.train(10)
      val inititalResultChangeAnno = modelChangeAnno.similaritiesAndLabels(mTrain, mTest)
      println("\nInitial MAP selected rules: " + Evaluator.meanAveragePrecision(inititalResultChangeAnno) + "\n")


      val mTrainNoAnno = mTrain.copy()
      val modelNoAnno = UniversalSchemaModel.randomModel(numRows, numCols, dim, new Random(seedForModels))
      val trainerNoAnno = new RegularizedBprUniversalSchemaTrainer(regularizer, stepsize, dim, mTrainNoAnno, modelNoAnno, new Random(seedForModels))
      trainerNoAnno.train(10)
      val initialResultNoAnno = modelNoAnno.similaritiesAndLabels(mTrainNoAnno, mTest)
      println("\nInitial MAP no annotation: " + Evaluator.meanAveragePrecision(initialResultNoAnno) + "\n")


      val mTrainRandomAnno = mTrain.copy()
      val modelRandomAnno = UniversalSchemaModel.randomModel(numRows, numCols, dim, new Random(seedForModels))
      val trainerRandomAnnotation = new RegularizedBprUniversalSchemaTrainer(regularizer, stepsize, dim, mTrainRandomAnno, modelRandomAnno, new Random(seedForModels))
      trainerRandomAnnotation.train(10)
      val initialResultRandomAnno = modelRandomAnno.similaritiesAndLabels(mTrainRandomAnno, mTest)
      println("\nInitial MAP random rules: " + Evaluator.meanAveragePrecision(initialResultRandomAnno) + "\n")
*/
/*
      var acceptedRulesHeuristic = 0.0
      var acceptedRulesRandom = 0.0
      var acceptedRulesSimilarity = 0.0
      var acceptedRulesChange = 0.0

      var filledCellsIfHoldsHeuristic = 0.0
      var filledCellsIfNotHoldsHeuristic = 0.0

      var filledCellsIfHoldsRandom = 0.0
      var filledCellsIfNotHoldsRandom = 0.0

      var filledCellsIfHoldsSimilarity = 0.0
      var filledCellsIfNotHoldsSimilarity = 0.0

      var filledCellsIfHoldsChange = 0.0
      var filledCellsIfNotHoldsChange = 0.0
*/
      for (targetIdx <- testCols) {
        println("current target column: " + targetIdx)

        for(row <- Range(0, mTrainGoldAnno.numRows())) {
          if (row % numTopics == targetIdx % numTopics) {
            mTrainGoldAnno.set(row, targetIdx, 1.0)
          }
        }

        val annoAntecedents = evaluatorAnno.model.columnToExpectedRankingGain(mTrainAnno, targetIdx, 2).toSeq.sortBy(-_._2).slice(0,numRulesProposed)
        evaluatorAnno.annotateHardConstraints(annoAntecedents.map(_._1), targetIdx, numTopics)

        val similarAntecedents = modelSimilarityAnno.columnToSimilarity(mTrainSimilarityAnno, targetIdx, 2).toSeq.sortBy(-_._2).slice(0,numRulesProposed)
        evaluatorSimilarity.annotateHardConstraints(similarAntecedents.map(_._1), targetIdx, numTopics)

        val changeAntecedents = modelChangeAnno.columnToChange(mTrainChangeAnno, targetIdx, 2).toSeq.sortBy(-_._2).slice(0,numRulesProposed)
        evaluatorChange.annotateHardConstraints(changeAntecedents.map(_._1), targetIdx, numTopics)

        val randomAntecedents = random.shuffle(modelRandomAnno.columnToExpectedRankingGain(mTrainRandomAnno, targetIdx, 2).toSeq).slice(0,numRulesProposed)
        evaluatorRandom.annotateHardConstraints(randomAntecedents.map(_._1), targetIdx, numTopics)

        /*val bestAntecedents = modelAnno.columnToFreq(mTrainAnno, targetIdx, 2).toSeq.sortBy(-_._2).slice(0,20)
        for (antecedentIdx <- bestAntecedents.map(_._1)) {
          val relationHolds = ((antecedentIdx % numTopics) == (targetIdx % numTopics))
          if (relationHolds) {
            acceptedRulesHeuristic += 1

            for(row <- mTrainAnno.colToRows.get(antecedentIdx).get) {
              // Get all nnz rows for antecedent
              val antecedentVal = mTrainAnno.get(row, antecedentIdx)

              if (antecedentVal == 1) {

                if (antecedentVal > mTrainAnno.get(row, targetIdx)) {
                  mTrainAnno.set(row, targetIdx, antecedentVal)
                  if (row % numTopics == antecedentIdx % numTopics) {
                    filledCellsIfHoldsHeuristic += 1
                  } else {
                    filledCellsIfNotHoldsHeuristic += 1
                  }
                }
              }
            }
          }
        }


        val similarAntecedents = modelSimilarityAnno.columnToSimilarity(mTrainSimilarityAnno, targetIdx, 2).toSeq.sortBy(-_._2).slice(0,numRulesProposed)
        for (antecedentIdx <- similarAntecedents.map(_._1)) {
          val relationHolds = ((antecedentIdx % numTopics) == (targetIdx % numTopics))
          if (relationHolds) {
            acceptedRulesSimilarity += 1

            for(row <- mTrainSimilarityAnno.colToRows.get(antecedentIdx).get) {
              // Get all nnz rows for antecedent
              val antecedentVal = mTrainSimilarityAnno.get(row, antecedentIdx)

              if (antecedentVal == 1) {

                if (antecedentVal > mTrainSimilarityAnno.get(row, targetIdx)) {
                  mTrainSimilarityAnno.set(row, targetIdx, antecedentVal)
                  if (row % numTopics == antecedentIdx % numTopics) {
                    filledCellsIfHoldsSimilarity += 1
                  } else {
                    filledCellsIfNotHoldsSimilarity += 1
                  }
                }
              }
            }
          }
        }


        val changeAntecedents = modelChangeAnno.columnToChange(mTrainChangeAnno, targetIdx, 2).toSeq.sortBy(-_._2).slice(0,numRulesProposed)
        for (antecedentIdx <- changeAntecedents.map(_._1)) {
          val relationHolds = ((antecedentIdx % numTopics) == (targetIdx % numTopics))
          if (relationHolds) {
            acceptedRulesChange += 1

            for(row <- mTrainChangeAnno.colToRows.get(antecedentIdx).get) {
              // Get all nnz rows for antecedent
              val antecedentVal = mTrainChangeAnno.get(row, antecedentIdx)

              if (antecedentVal == 1) {

                if (antecedentVal > mTrainChangeAnno.get(row, targetIdx)) {
                  mTrainChangeAnno.set(row, targetIdx, antecedentVal)
                  if (row % numTopics == antecedentIdx % numTopics) {
                    filledCellsIfHoldsChange += 1
                  } else {
                    filledCellsIfNotHoldsChange += 1
                  }
                }
              }
            }
          }
        }
*/
        println("===")
        println("best antecedents:")
        println("ERI: " + bestAntecedents)
        println("change: " + changeAntecedents)
        println("similarity: " + similarAntecedents)

/*
        val randomAntecedents = random.shuffle(modelRandomAnno.columnToExpectedRankingGain(mTrainRandomAnno, targetIdx, 2).toSeq).slice(0,numRulesProposed)
        for (antecedentIdx <- randomAntecedents.map(_._1)) {
          val relationHolds = ((antecedentIdx % numTopics) == (targetIdx % numTopics))
          if (relationHolds) {
            acceptedRulesRandom += 1

            for(row <- mTrainRandomAnno.colToRows.get(antecedentIdx).get) {
              // Get all nnz rows for antecedent
              val antecedentVal = mTrainRandomAnno.get(row, antecedentIdx)
              if (antecedentVal == 1) {
                if (antecedentVal > mTrainRandomAnno.get(row, targetIdx)) {
                  mTrainRandomAnno.set(row, targetIdx, antecedentVal)
                  if (row % numTopics == antecedentIdx % numTopics) {
                    filledCellsIfHoldsRandom += 1
                  } else {
                    filledCellsIfNotHoldsRandom += 1
                  }
                }
              }
            }
          }
        }
        */
      }


      println("SUGGESTED HERUISTIC:")
      println("Antecedent cells following pattern: " + filledCellsIfHoldsHeuristic)
      println("Antecedent cells not following pattern: " + filledCellsIfNotHoldsHeuristic)
      println("===")

      println("SIMILARITY ONLY:")
      println("Antecedent cells following pattern: " + filledCellsIfHoldsSimilarity)
      println("Antecedent cells not following pattern: " + filledCellsIfNotHoldsSimilarity)
      println("===")

      println("CHANGE ONLY:")
      println("Antecedent cells following pattern: " + filledCellsIfHoldsChange)
      println("Antecedent cells not following pattern: " + filledCellsIfNotHoldsChange)
      println("===")

      println("RANDOM SELECTION:")
      println("Antecedent cells following pattern: " + filledCellsIfHoldsRandom)
      println("Antecedent cells not following pattern: " + filledCellsIfNotHoldsRandom)
      println("===")

      println("\ntraining gold annotations:")
      trainerGoldAnnotation.train(10)
      println("\ntraining heuristic annotations:")
      trainerForAnnotation.train(10)
      println("\ntraining random annotations:")
      trainerRandomAnnotation.train(10)
      println("\ntraining no annotations:")
      trainerNoAnno.train(10)
      println("\ntraining similarity annotations:")
      trainerForSimilarityAnnotation.train(10)
      println("\ntraining change annotations:")
      trainerForChangeAnnotation.train(10)

      // Note: we are using mTrain here, in order to allow for annotated cells to have direct positive (or negative) impact.
      val resultGoldAnno = modelGoldAnnotation.similaritiesAndLabels(mTrain, mTest)
      val resultAnno = modelAnno.similaritiesAndLabels(mTrain, mTest)
      val resultRandomAnno = modelRandomAnno.similaritiesAndLabels(mTrain, mTest)
      val resultNoAnno = modelNoAnno.similaritiesAndLabels(mTrain, mTest)
      val resultSimilarityAnno = modelSimilarityAnno.similaritiesAndLabels(mTrain, mTest)
      val resultChangeAnno = modelChangeAnno.similaritiesAndLabels(mTrain, mTest)

      resultsGoldAnno = Evaluator.meanAveragePrecision(resultGoldAnno) :: resultsGoldAnno
      resultsAnno = Evaluator.meanAveragePrecision(resultAnno) :: resultsAnno
      resultsRandomAnno = Evaluator.meanAveragePrecision(resultRandomAnno) :: resultsRandomAnno
      resultsNoAnno = Evaluator.meanAveragePrecision(resultNoAnno) :: resultsNoAnno
      resultsSimilarityAnno = Evaluator.meanAveragePrecision(resultSimilarityAnno) :: resultsSimilarityAnno
      resultsChangeAnno = Evaluator.meanAveragePrecision(resultChangeAnno) :: resultsChangeAnno

      rulesAppliedAnno = acceptedRulesHeuristic :: rulesAppliedAnno
      rulesAppliedRandomAnno = acceptedRulesRandom :: rulesAppliedRandomAnno
      rulesAppliedChangeAnno = acceptedRulesChange :: rulesAppliedChangeAnno
      rulesAppliedSimilarityAnno = acceptedRulesSimilarity :: rulesAppliedSimilarityAnno

      cellsFilledCorrectAnno = filledCellsIfHoldsHeuristic :: cellsFilledCorrectAnno
      cellsFilledCorrectRandomAnno = filledCellsIfHoldsRandom :: cellsFilledCorrectRandomAnno

      cellsFilledIncorrectAnno = filledCellsIfNotHoldsHeuristic :: cellsFilledIncorrectAnno
      cellsFilledIncorrectRandomAnno = filledCellsIfNotHoldsRandom :: cellsFilledIncorrectRandomAnno

      cellsFilledCorrectChangeAnno = filledCellsIfHoldsChange :: cellsFilledCorrectChangeAnno
      cellsFilledIncorrectChangeAnno = filledCellsIfNotHoldsChange :: cellsFilledIncorrectChangeAnno

      cellsFilledCorrectSimilarityAnno = filledCellsIfHoldsSimilarity :: cellsFilledCorrectSimilarityAnno
      cellsFilledIncorrectSimilarityAnno = filledCellsIfNotHoldsSimilarity :: cellsFilledIncorrectSimilarityAnno


      println("MAP gold annotation: " + Evaluator.meanAveragePrecision(resultGoldAnno))
      println("MAP selected rules: " + Evaluator.meanAveragePrecision(resultAnno))
      println("MAP random rules: " + Evaluator.meanAveragePrecision(resultRandomAnno))
      println("MAP no annotation: " + Evaluator.meanAveragePrecision(resultNoAnno))
      println("MAP similarity: " + Evaluator.meanAveragePrecision(resultSimilarityAnno))
      println("MAP change: " + Evaluator.meanAveragePrecision(resultChangeAnno))
      println("===")
    }

    println("===")
    println("Mean and standard error:")
    println("gold annotation: " + meanAndStandardError(resultsGoldAnno))
    println("selected rules: " + meanAndStandardError(resultsAnno))
    println("random rules: " + meanAndStandardError(resultsRandomAnno))
    println("no annotation: " + meanAndStandardError(resultsNoAnno))
    println("similarity annotation: " + meanAndStandardError(resultsSimilarityAnno))
    println("change annotation: " + meanAndStandardError(resultsChangeAnno))

    println("===")
    println("Rules Applied (out of " + numRulesProposed + " proposed)")
    println("heuristic: " + meanAndStandardError(rulesAppliedAnno))
    println("random: " + meanAndStandardError(rulesAppliedRandomAnno))
    println("similarity: " + meanAndStandardError(rulesAppliedSimilarityAnno))
    println("change: " + meanAndStandardError(rulesAppliedChangeAnno))
    println("===")
    println("Cells correctly filled:")
    println("heuristic: " + meanAndStandardError(cellsFilledCorrectAnno))
    println("random: " + meanAndStandardError(cellsFilledCorrectRandomAnno))
    println("similarity: " + meanAndStandardError(cellsFilledCorrectSimilarityAnno))
    println("change: " + meanAndStandardError(cellsFilledCorrectChangeAnno))
    println("===")
    println("Cells incorrectly filled:")
    println("heuristic: " + meanAndStandardError(cellsFilledIncorrectAnno))
    println("random: " + meanAndStandardError(cellsFilledIncorrectRandomAnno))
    println("similarity: " + meanAndStandardError(cellsFilledIncorrectSimilarityAnno))
    println("change: " + meanAndStandardError(cellsFilledIncorrectChangeAnno))
  }
}
