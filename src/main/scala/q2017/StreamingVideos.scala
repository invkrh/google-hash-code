package q2017

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
 * Step 1: Read statement, define IO interfaces and push (30 mins)
 */
// Input
case class EndPoint(
  ld: Int,
  cacheLatency: Array[Int],
  videoRequestNumber: java.util.Map[Integer, Integer]
)

case class Input(
  v: Int,
  e: Int,
  r: Int,
  c: Int,
  x: Int,
  videoSizes: Array[Int],
  endPoints: Array[EndPoint]
)

object Input {

  /**
   * Step 2b: Implement IO interfaces, check parsing and output format and push (30 mins)
   */
  def apply(path: String): Input = {
    val lines = Source.fromResource(path).getLines
    val Array(v, e, r, c, x) = lines.next().split(" ").map(_.toInt)
    val videoSizes = lines.next().split(" ").map(_.toInt)
    val endPoints = new Array[EndPoint](e)
    for (i <- 0 until e) {
      val Array(ld, nbCaches) = lines.next().split(" ").map(_.toInt)
      val cacheLatency = new Array[Int](nbCaches)
      for (_ <- 0 until nbCaches) {
        val Array(cacheId, latency) = lines.next().split(" ").map(_.toInt)
        cacheLatency(cacheId) = latency
      }
      endPoints(i) = EndPoint(ld, cacheLatency, new util.HashMap[Integer, Integer]())
    }

    for (_ <- 0 until r) {
      val Array(videoId, endPointId, reqNumb) = lines.next().split(" ").map(_.toInt)
      endPoints(endPointId).videoRequestNumber.put(videoId, reqNumb)
    }

    Input(v, e, r, c, x, videoSizes, endPoints)
  }
}

// Output
case class Output(cache2videos: Array[Array[Int]]) {
  override def toString: String =
    cache2videos.length + "\n" + cache2videos.zipWithIndex
      .filter(_._1.nonEmpty)
      .map {
        case (videos, cacheId) => (cacheId :: videos.toList).mkString(" ")
      }
      .mkString("\n")
  def save(path: String): Path =
    Files.write(Paths.get(path), this.toString.getBytes(StandardCharsets.UTF_8))
}

object StreamingVideas extends App {
  val basePath = "q2017"

  /**
   * Step 3b: Implement the second solution (should be different) and validate with real data
   */
  def solve(input: Input): Output = {
    Output(Array(Array(2), Array(3, 1), Array(0, 1)))
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

    val input = Input(s"$basePath/$fileName.in")
    println(input)

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
  List("example") foreach run
}
