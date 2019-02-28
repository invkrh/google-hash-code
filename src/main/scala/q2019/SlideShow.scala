package q2019

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import scala.io.Source

/**
 * Step 1: Read statement, define IO interfaces and push (30 mins)
 */
// Input
case class Photo(dir: Char, tags: Array[String])
case class Input(photos: Array[Photo])

object Input {

  /**
   * Step 2b: Implement IO interfaces, check parsing and output format and push (30 mins)
   */
  def apply(path: String): Input = {
    val lines = Source.fromResource(path).getLines
    val n = lines.next().toInt
    val photos = new Array[Photo](n)
    for (i <- 0 until n) {
      val tokens = lines.next().split(" ")
      val dir = tokens(0).toCharArray.head
      photos(i) = Photo(dir, tokens.drop(2))
    }
    new Input(photos)
  }
}

// Output
case class Output(slides: Array[Array[Int]]) {
  override def toString: String = slides.size + "\n" + slides.map(_.mkString(" ")).mkString("\n")
  def save(path: String): Path =
    Files.write(Paths.get(path), this.toString.getBytes(StandardCharsets.UTF_8))
}

object SlideShow extends App {
  val basePath = "q2019"

  /**
   * Step 3b: Implement the second solution (should be different) and validate with real data
   */
  def solve(input: Input): Output = {
    Output(Array(Array(0), Array(3), Array(1, 2)))
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
  val fileList = List(
    "a_example",
    "b_lovely_landscapes",
    "c_memorable_moments",
    "d_pet_pictures",
    "e_shiny_selfies")
  val dataSet = fileList.map(fileName => (fileName, Input(s"$basePath/$fileName.txt"))).toMap
  fileList foreach run
}
