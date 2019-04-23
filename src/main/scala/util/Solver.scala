package util
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import scala.io.Source

trait Solver {

  type In
  type Out

  def round: String
  def getInput(lines: Iterator[String]): In
  def solve(in: In): Out
  def validate(in: In, out: Out): Unit
  def computeScore(in: In, out: Out): Int

  def time[T](action: => T): T = {
    val start = System.currentTimeMillis()
    val res = action
    println("Time = " + (System.currentTimeMillis() - start) + " ms")
    res
  }

  def run(caseName: String, showInput: Boolean = false): Int = {
    println(s"Case: $caseName")
    print("Reading input ... ")
    val lineIter = Source.fromFile(s"data/$round/in/$caseName.txt").getLines
    val in = getInput(lineIter)
    println("done")
    if (showInput) { println(in) }
    val out = time { solve(in) }
    validate(in, out)
    val score = computeScore(in, out)
    println("Score = " + score)
    println
    Files.write(
      Paths.get(s"data/$round/out/${caseName}_$score.txt"),
      out.toString.getBytes(StandardCharsets.UTF_8))
    score
  }
}
