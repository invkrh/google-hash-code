package qualification2018

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.text.SimpleDateFormat

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source
import scala.util.Random

object Main extends App {

  import scala.math._
  // Data structure
  case class Ride(id: Int, a: Int, b: Int, x: Int, y: Int, s: Int, f: Int) {
    val points = abs(a - b) + abs(x - y)
  }
  case class Input(R: Int, C: Int, F: Int, N: Int, B: Int, T: Int, rides: ListBuffer[Ride])
  object Input {
    def apply(path: String): Input = {
      val lines = Source.fromResource(path).getLines
      val Array(r, c, f, n, b, t) = lines.next().split(" ").map(_.toInt)
      Input(r, c, f, n, b, t, ListBuffer.tabulate(n) { i =>
        val Array(a, b, x, y, s, f) = lines.next().split(" ").map(_.toInt)
        Ride(i, a, b, x, y, s, f)
      })
    }
  }

  case class Vehicle(
    var posX: Int = 0,
    var posY: Int = 0,
    ridesTaken: ArrayBuffer[Ride] = new ArrayBuffer()
  ) {

    var currentRide: Option[Ride] = None
    var finishTime = 0

    def dist2Start(r: Ride): Int = abs(posX - r.a) + abs(posY - r.b)

    def computeScore(r: Ride, t: Int): Double = {
      val finishTime =
        if (t + dist2Start(r) <= r.s) r.points + r.s
        else
          t + r.points + dist2Start(r)
      if (finishTime > r.f) 0d
      else {
        val bonus = if (t + this.dist2Start(r) <= r.s) input.B else 0
        val value = r.points + bonus
        val cost = r.points + (if (t + this.dist2Start(r) <= r.s) r.s - t else this.dist2Start(r))
        value / cost.toDouble
      }
    }

    def randomPick[T](in: Seq[T]): T = {
      in.apply(Random.nextInt(in.size))
    }

    def selectRide(t: Int): Unit = {
      if (input.rides.nonEmpty) {
//        val roiMap = input.rides.map(r => (r, computeROI(r, t))).toMap
//        val maxROI = roiMap.values.max
//        val eligible = roiMap.filter(_._2 == maxROI).keys.toSeq
//        val ride = randomPick(eligible)
        val ride = input.rides.maxBy(r => computeScore(r, t))
        input.rides -= ride
        this.currentRide = Some(ride)
        this.ridesTaken.append(ride)
        finishTime =
          if (t + dist2Start(ride) <= ride.s) ride.points + ride.s
          else
            t + ride.points + dist2Start(ride)
      }
    }

    def update(t: Int): Unit = {
      currentRide match {
        case Some(ride) =>
          if (t == finishTime) {
            currentRide = None
            this.posX = ride.x
            this.posY = ride.y
            selectRide(t)
          }
        case None =>
          selectRide(t)
      }
    }
  }

  case class Output(vehicles: Array[Vehicle]) {
    override def toString: String =
      vehicles
        .map { v =>
          v.ridesTaken.size + " " + v.ridesTaken.map(_.id).mkString(" ")
        }
        .mkString("\n")

    def score(): Int = 0

    def save(): Path = {
      println("Score: " + score())
      println(this.toString)
      val timeFormat = new SimpleDateFormat("HH-mm-ss")
      val suffix = timeFormat.format(new java.util.Date())
      val path = s"output/$prefix/$problem-hao-$suffix.out"
      Files.write(Paths.get(path), this.toString.getBytes(StandardCharsets.UTF_8))
    }
  }

  val prefix = "qualification2018"

  var problem = "a_example"
  var input = Input(s"$prefix/$problem.in")

  List("b_should_be_easy", "c_no_hurry", "d_metropolis", "e_high_bonus") foreach { p =>
    problem = p
    input = Input(s"$prefix/$problem.in")
    val vehicles = Array.fill(input.F)(Vehicle())
    for {
      t <- 0 until input.T
      v <- vehicles
    } {
      v.update(t)
    }
    val output: Output = Output(vehicles)
    output.save()
  }
}
