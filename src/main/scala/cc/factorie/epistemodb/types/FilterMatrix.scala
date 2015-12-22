package cc.factorie.epistemodb.types

import scala.util.Random
import cc.factorie.epistemodb._

/**
 * Created by beroth on 2/23/15.
 */

class FilterMatrixOptions extends cc.factorie.util.DefaultCmdOptions {
  val matrix = new CmdOption("matrix", "", "FILE", "tab separated file with TAC training data")
  val filtered = new CmdOption("filtered", "", "FILE", "tab separated file with TAC training data")
  val pruning = new CmdOption("pruning", 10, "INT", "amount of pruning")
}

// #MAVEN_OPTS="-Xmx100g" m mvn exec:java -Dexec.mainClass="cc.factorie.epistemodb.types.TrainTestTacData" -Dexec.args="--matrix=/iesl/canvas/beroth/data/tmp/contexts /iesl/canvas/beroth/data/tmp/contexts.filtered"
object FilterMatrix {
  val opts = new FilterMatrixOptions
    def main(args: Array[String]) : Unit = {
      opts.parse(args)

      val tReadStart = System.currentTimeMillis
//      val kb = EntityRelationKBMatrix.fromTsv(opts.tacData.value).prune(2,1)

      val kb = StringStringKBMatrix.fromRowColumnTsvMinFreq(opts.matrix.value, opts.pruning.value).prune(
        opts.pruning.value, opts.pruning.value)
      //val kb = StringStringKBMatrix.fromRowColumnTsv(opts.matrix.value).prune(2,1)

      val tRead = (System.currentTimeMillis - tReadStart)/1000.0
      println(f"Reading from file and pruning took $tRead%.2f s")

      println("Stats:")
      println("Num Rows:" + kb.numRows())
      println("Num Cols:" + kb.numCols())
      println("Num cells:" + kb.nnz())

      kb.writeToTsvFile(opts.filtered.value)
    }
}
