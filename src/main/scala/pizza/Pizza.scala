package pizza

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import scala.io.Source

/**
 * Step 1: Read statement, define IO interfaces and push (30 mins)
 */
// Input
case class Input(r: Int, c: Int, l: Int, h: Int, pizza: Array[Array[Char]])
object Input {

  /**
   * Step 2b: Implement IO interfaces, check parsing and output format and push (30 mins)
   */
  def apply(path: String): Input = {
    val lines = Source.fromResource(path).getLines
    val Array(r, c, l, h) = lines.next().split(" ").map(_.toInt)
    val pizza = Array.ofDim[Char](r, c)
    for {
      i <- 0 until r
    } {
      pizza(i) = lines.next.toCharArray
    }
    Input(r, c, l, h, pizza)
  }
}

// Output
case class Slice(r1: Int, c1: Int, r2: Int, c2: Int) {
  override def toString: String = {
    List(r1, c1, r2, c2).mkString(" ")
  }
}
case class Output(s: Int, slices: Array[Slice]) {
  override def toString: String = s + "\n" + slices.mkString("\n")
  def save(path: String): Path =
    Files.write(Paths.get(path), this.toString.getBytes(StandardCharsets.UTF_8))
}

object Pizza extends App {
  val basePath = "pizza"

  /**
   * Step 3b: Implement the second solution (should be different) and validate with real data
   */
  def solve(input: Input): Output = {
    Output(3, Array(Slice(0, 0, 2, 1), Slice(0, 2, 2, 2), Slice(0, 3, 2, 4)))
  }

  def run(fileName: String): Output = {

    val input = Input(s"$basePath/$fileName")

    // Java solver
    // Solver.solve(input)

    // Scala solver
    solve(input)
  }

  /**
   * Step 4: Upload output files to gain points
   */
  List("a_example", "b_small", "c_medium", "d_big") foreach { fileName =>
    run(fileName + ".in").save(s"output/$basePath/$fileName.out")
  }
}
