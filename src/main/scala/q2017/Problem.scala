package q2017

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import scala.io.Source

/**
 * Step 1: Read statement, define IO interfaces and push (30 mins)
 */
// Input
case class EndPoints(
  cacheLatency: java.util.Map[Int, Int],
  videoRequestNumber: java.util.Map[Int, Int]
)
case class Input(v: Int, e: Int, r: Int, c: Int, x: Int, endpoints: Array[EndPoints])
object Input {

  /**
   * Step 2b: Implement IO interfaces, check parsing and output format and push (30 mins)
   */
  def apply(path: String): Input = {
    val lines = Source.fromResource(path).getLines
    ???
  }
}

// Output
case class Output(cache2videos: java.util.Map[Int, Array[Int]]) {
  override def toString: String = ???
  def save(path: String): Path =
    Files.write(Paths.get(path), this.toString.getBytes(StandardCharsets.UTF_8))
}

object StreamingVideas extends App {
  val basePath = "template"

  /**
   * Step 3b: Implement the second solution (should be different) and validate with real data
   */
  def solve(input: Input): Output = {
    ???
  }

  /**
   *  Optional
   */
  def validate(input: Input, output: Output): Boolean = {
    ???
  }

  /**
   *  Optional
   */
  def computeScore(input: Input, output: Output): Int = {
    ???
  }

  def run(fileName: String): Unit = {

    val input = Input(s"$basePath/$fileName.in")
    println(input)

    // Java solver
    val output = Solver.solve(input)

    // Scala solver
    // val output = solve(input)

    val score = computeScore(input, output)

    println(s"$fileName = " + score)
    output.save(s"output/$basePath/$fileName.out")
  }

  /**
   * Step 4: Upload output files to gain points
   */
  List() foreach run
}
