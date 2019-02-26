package template

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
  val basePath = "template"

  /**
   * Step 3b: Implement the second solution (should be different) and validate with real data
   */
  def solve(input: Input): Output = {
    ???
  }

  def run(fileName: String): Output = {

    val input = Input(s"$basePath/$fileName")

    // Java solver
    Solver.solve(input)

    // Scala solver
    // solve(input)
  }

  /**
   * Step 4: Upload output files to gain points
   */
  List("???", "???", "???", "???", "???", "???") foreach { fileName =>
    run(fileName + ".in").save(s"output/$basePath/$fileName.out")
  }
}
