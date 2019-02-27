package pizza

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
 * Step 1: Read statement, define IO interfaces and push (30 mins)
 */
// Input
case class Input(r: Int, c: Int, l: Int, h: Int, pizza: Array[Array[Char]]) {
  override def toString() = {
    List(r, c, l, h).mkString(" ") + "\n" + pizza.map(_.mkString("")).mkString("\n")
  }
}
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
import math._
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

  def validate(slice: Slice, input: Input): Boolean = {
    slice match {
      case Slice(r1, c1, r2, c2) =>
        var cntT = 0
        var cntM = 0
        for {
          i <- min(r1, r2) to max(r1, r2)
          j <- min(c1, c2) to max(c1, c2)
        } {
          if (input.pizza(i)(j) == 'T') {
            cntT += 1
          } else {
            cntM += 1
          }
        }
        cntT >= input.l && cntT <= input.h && cntM >= input.l && cntM <= input.h
    }
  }

  def computeScore(input: Input, output: Output): Int = {

    def isOverlapped(s1: Slice, s2: Slice): Boolean = {
      if (s1.r1 > s2.r2 || s2.r1 > s1.r2) {
        false
      } else if (s1.c1 < s2.c2 || s2.c1 < s1.c2) {
        false
      } else {
        true
      }
    }

    output.slices foreach { s =>
      if (!validate(s, input))
        throw new Exception("L, H are not satisfied")
    }

    for {
      s1 <- output.slices
      s2 <- output.slices.filter(_ != s1)
    } yield {
      if (isOverlapped(s1, s2)) throw new Exception(s"Slices overlaped: $s1 and $s2")
    }

    output.slices.map {
      case Slice(r1, c1, r2, c2) =>
        (math.abs(r1 - r2) + 1) * (math.abs(c1 - c2) + 1)
    }.sum
  }

  /**
   * Step 3b: Implement the second solution (should be different) and validate with real data
   */
  def solve(input: Input): Output = {
    val buffer = ArrayBuffer[Slice]()

    val masks = (for {
      i <- 1 to input.h
      j <- 1 to input.h if i * j >= 2 * input.l && i * j <= input.h
    } yield (i, j)).sortBy(p => -p._1 * p._2)

    masks foreach cutWithMask

    def cutWithMask(mask: (Int, Int)): Unit = {
      ???
    }

    Output(buffer.size, buffer.toArray)
  }

  def run(fileName: String): Unit = {

    val input = Input(s"$basePath/$fileName.in")
    println(input)

    // Java solver
    // Solver.solve(input)

    // Scala solver
    val output = solve(input)

    val score = computeScore(input, output)

    println(s"$fileName = " + score)
    output.save(s"output/$basePath/$fileName.out")
  }

  /**
   * Step 4: Upload output files to gain points
   */
  List("a_example", "b_small", "c_medium", "d_big") foreach run
}
