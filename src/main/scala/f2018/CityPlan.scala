package f2018

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

// Input
case class Point(x: Int, y: Int)
case class BuildingProject(tp: Char, hp: Int, wp: Int, value: Int, occupied: Array[Point])
case class Input(H: Int, W: Int, D: Int, B: Int, buildProjects: Array[BuildingProject])
object Input {
  def apply(path: String): Input = {
    val lines = Source.fromResource(path).getLines
    val Array(h, w, d, b) = lines.next().split(" ").map(_.toInt)

    val buildProjects = Array.tabulate(b) { i =>
      val Array(tp, hp, wp, value) = lines.next().split(" ")
      val occupied = ArrayBuffer[Point]()
      List.tabulate(hp.toInt) { row =>
        lines.next().toCharArray.zipWithIndex.foreach {
          case (used, col) =>
            if (used == '#') {
              occupied.append(Point(row, col))
            }
        }
      }
      BuildingProject(tp.head, hp.toInt, wp.toInt, value.toInt, occupied.toArray)
    }
    Input(h, w, d, b, buildProjects)
  }
}

// Output
case class Building(b: Int, r: Int, c: Int) {
  override def toString: String = Array(b, r, c).mkString(" ")
}
case class Output(N: Int, buildings: Array[Building]) {
  override def toString: String = N + "\n" + buildings.mkString("\n")
  def save(path: String): Path = {
    Files.write(Paths.get(path), this.toString.getBytes(StandardCharsets.UTF_8))
  }
}

object CityPlan extends App {

  val basePath = "f2018"

  def solve(city: Input): Output = {
    Output(4, Array(Building(0, 0, 0), Building(1, 3, 0), Building(2, 0, 2), Building(0, 0, 5)))
  }

  def run(fileName: String): Output = {

    val input = Input(s"$basePath/$fileName")

    // Java solver
    Solver.solve(input)

    // Scala solver
    solve(input)
  }

  List(
    "a_example",
    "b_short_walk",
    "c_going_green",
    "d_wide_selection",
    "e_precise_fit",
    "f_different_footprints"
  ) foreach { fileName =>
    run(fileName + ".in").save(s"output/$basePath/$fileName.out")
  }
}
