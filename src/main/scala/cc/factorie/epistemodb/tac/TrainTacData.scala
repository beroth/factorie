package cc.factorie.epistemodb.tac

import cc.factorie.epistemodb._
import scala.util.Random
import scala.Some
import scala.io.Source

/**
 * Created by beroth on 4/16/15.
 */
class TrainTacDataOptions extends cc.factorie.util.DefaultCmdOptions {
  val tacData = new CmdOption("tac-data", "", "FILE", "tab separated file with TAC training data")
  val dim = new CmdOption("dim", 100, "INT", "dimensionality of data")
  val iters = new CmdOption("iterations", 200, "INT", "dimensionality of data")
  val stepsize = new CmdOption("stepsize", 0.1, "DOUBLE", "step size")
  val regularizer = new CmdOption("regularizer", 0.01, "DOUBLE", "regularizer")
  val relations = new CmdOption("relations", "", "FILE", "Relations for which patterns are to be written out.")
  val patternsOut = new CmdOption("patterns-out", "", "FILE", "Top-scored columns, for test columns.")
}

object TrainTacData {
  val opts = new TrainTacDataOptions

  def main(args: Array[String]) : Unit = {
    opts.parse(args)
    val tReadStart = System.currentTimeMillis
    //      val kb = EntityRelationKBMatrix.fromTsv(opts.tacData.value).prune(2,1)
    val kb = StringStringKBMatrix.fromTsv(opts.tacData.value).prune(2,1)
    val tRead = (System.currentTimeMillis - tReadStart)/1000.0
    println(f"Reading from file and pruning took $tRead%.2f s")
    println("Stats:")
    println("Num Rows:" + kb.numRows())
    println("Num Cols:" + kb.numCols())
    println("Num cells:" + kb.nnz())
    val random = new Random(0)
    val model = UniversalSchemaModel.randomModel(kb.numRows(), kb.numCols(), opts.dim.value, random)
    val trainer = new RegularizedBprUniversalSchemaTrainer(opts.regularizer.value, opts.stepsize.value, opts.dim.value,
        kb.matrix, model, random)
    trainer.train(opts.iters.value)
    val testCols = Source.fromFile(opts.relations.value).getLines().toSet
    kb.writeTopPatterns(testCols, model, 0.5, opts.patternsOut.value)
  }

}
