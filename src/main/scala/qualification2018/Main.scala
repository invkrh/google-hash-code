package qualification2018

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.text.SimpleDateFormat
import java.time.LocalDateTime

import scala.io.Source

object Main extends App {

  val prefix = "qualification2018"

  // Data structure
  case class Input()
  object Input {
    def apply(path: String): Input = {
      val lines = Source.fromResource(s"$prefix/example.in").getLines
      ???
    }
  }

  case class Output() {
    override def toString: String = ???

    def score(): Int = {
      input
      ???
    }

    def save(): Path = {
      println("Score: " + score())
      println(this.toString)
      val timeFormat = new SimpleDateFormat("HH-mm-ss")
      val suffix = timeFormat.format(new java.util.Date())
      val path = s"output/$prefix/heuristic-hao-$suffix.out"
      Files.write(Paths.get(path), this.toString.getBytes(StandardCharsets.UTF_8))
    }
  }

  val problem: String = ???
  val input = Input(s"$prefix/$problem.in")

  // TODO: Fill solution

  val output: Output = ???

  output.save()
}
