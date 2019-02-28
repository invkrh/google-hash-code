package q2019

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import scala.io.Source

/**
 * Step 1: Read statement, define IO interfaces and push (30 mins)
 */
// Input
case class Input()
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
case class Output() {
  override def toString: String = ???
  def save(path: String): Path =
    Files.write(Paths.get(path), this.toString.getBytes(StandardCharsets.UTF_8))
}

object Problem extends App {
  val basePath = "q2019"

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
    0
  }

  def run(fileName: String): Unit = {

    val input = dataSet(fileName)

    // Java solver
    // val output = Solver.solve(input)

    // Scala solver
    val output = solve(input)

    val score = computeScore(input, output)

    println(s"$fileName = " + score)
    output.save(s"output/$basePath/$fileName.out")
  }

  /**
   * Step 4: Upload output files to gain points
   */
  val fileList = List("example")
  val dataSet = fileList.map(fileName => (fileName, Input(s"$basePath/$fileName.in"))).toMap
  fileList foreach run
}
