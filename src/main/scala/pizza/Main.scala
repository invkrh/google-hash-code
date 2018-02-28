package pizza

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.time.LocalDateTime

import scala.io.Source

object Main extends App {

  // Data structure
  type Pizza = List[List[Char]]

  case class Input(R: Int, C: Int, L: Int, H: Int, pizza: Pizza) {
    def showPizza(): Unit = pizza.map(_ mkString " ") foreach println
  }

  case class Slice(r1: Int, c1: Int, r2: Int, c2: Int) {
    import Math.{max, min}
    override def toString = s"$r1 $c1 $r2 $c2"

    def points: List[(Int, Int)] = List((r1, c1), (r2, c2), (r1, c2), (r2, c1))

    def contains(x: Int, y: Int): Boolean = {
      x >= min(r1, r2) && x <= max(r1, r2) &&
      y >= min(c1, c2) && y <= max(c1, c2)
    }

    def isOverlap(slice: Slice): Boolean = {
      this.points.exists {
        case (x, y) => slice.contains(x, y)
      }
    }

    def cellSum(input: Input): Long = {
      if (r1 < 0 || r1 > input.R ||
        r2 < 0 || r2 > input.R ||
        c1 < 0 || c1 > input.C ||
        c2 < 0 || c2 > input.C) {
        throw new Exception("wrong boundary")
        0
      } else {
        val ingredients = for {
          i <- r1 to r2
          j <- c1 to c2
        } yield {
          input.pizza(i)(j).toString
        }
        if (ingredients.count(_ == "T") < input.L ||
          ingredients.count(_ == "M") < input.L ||
          ingredients.size > input.H) {
          throw new Exception("wrong counters")
          0
        } else {
          ingredients.size
        }
      }
    }
  }

  case class Solution(num: Int, slices: List[Slice]) {
    override def toString: String =
      num + "\n" + slices.map(_.toString).mkString("\n")
    def score(input: Input): Long = {
      val verdicts = for {
        s1 <- slices
        s2 <- slices if s1 != s2
      } yield {
        s1.isOverlap(s2)
      }
      if (slices.size != num) {
        throw new Exception("wrong slices size")
        0
      } else if (verdicts.contains(true)) {
        throw new Exception("overlapped")
        0
      } else {
        slices.foldLeft(0L) {
          case (acc, elem) => acc + elem.cellSum(input)
        }
      }
    }
    def toFile(path: String): Path = {
      Files.write(Paths.get(path), this.toString.getBytes(StandardCharsets.UTF_8))
    }
  }

  // Input
  val lines = Source.fromResource("pizza/example.in").getLines
  val Array(r, c, l, h) = lines.next().split(" ").map(_.toInt)
  val pizza = lines.map(_.toCharArray.toList).toList
  val input = Input(r, c, l, h, pizza)
  input.showPizza()

  // Heuristic
  val solution = Solution(3, List(Slice(0, 0, 2, 1), Slice(0, 2, 2, 2), Slice(0, 3, 2, 4)))
  println("Score: " + solution.score(input))

  // Output
  println(solution.toString)
  val now = LocalDateTime.now()
  val hour = now.getHour
  val minute = now.getMinute
  val second = now.getSecond
  solution.toFile(s"output/pizza/heuristic-hao-$hour-$minute-$second.out")
}
