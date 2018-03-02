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
    val dist: Int = abs(x - a) + abs(y - b)
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

    var currentScore = 0
    var currentRide: Option[Ride] = None
    var currentRideEndTime = 0

    def dist2Start(r: Ride): Int = abs(posX - r.a) + abs(posY - r.b)

    def computeROI(t: Int, r: Ride): Double = {
      if (finishTime(t, r) > r.f) 0d
      else { // can finish before f
        val value = r.dist + bonus(t, r)
        val cost = if (t + this.dist2Start(r) <= r.s) {
//           r.s - t // general case
          val waitTime = 1000 * (r.s - (t + this.dist2Start(r))) // penalize wait time
          waitTime + this.dist2Start(r)
        } else {
          this.dist2Start(r)
        }
        value / cost.toDouble
      }
    }

    def selectRide(t: Int): Unit = {
      if (input.rides.nonEmpty) {
        val ride = input.rides.maxBy(r => computeROI(t, r))
        if (computeROI(t, ride) != 0) {
          input.rides -= ride
          this.ridesTaken.append(ride)
          this.currentRide = Some(ride)
          this.currentRideEndTime = finishTime(t, ride)
          this.currentScore += bonus(t, ride)
        }
      }
    }

    def bonus(t: Int, r: Ride): Int = {
      if (t + this.dist2Start(r) <= r.s) input.B else 0
    }

    def finishTime(t: Int, r: Ride): Int = {
      if (t + dist2Start(r) <= r.s) {
        r.dist + r.s
      } else {
        t + r.dist + dist2Start(r)
      }
    }

    def update(t: Int): Unit = {
      currentRide match {
        case Some(ride) =>
          if (t == currentRideEndTime) {
            this.currentScore += ride.dist
            currentRide = None
            this.posX = ride.x
            this.posY = ride.y
            selectRide(t)
          }
        case None => // Init
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

    def score(): Int = vehicles.map(_.currentScore).sum

    def save(): Path = {
      println(s"Score ($problem): " + score())
      val timeFormat = new SimpleDateFormat("HH-mm-ss")
      val suffix = timeFormat.format(new java.util.Date())
      val path = s"output/$prefix/$problem-hao-$suffix.out"
      Files.write(Paths.get(path), this.toString.getBytes(StandardCharsets.UTF_8))
    }
  }

  val prefix = "qualification2018"

  var problem = "a_example"
  var input = Input(s"$prefix/$problem.in")

  val finalScores =
    List("a_example", "b_should_be_easy", "c_no_hurry", "d_metropolis", "e_high_bonus") map { p =>
//  List("b_should_be_easy") foreach { p =>
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
      println(s"Rides missed: ${input.rides.size} (${input.rides.size / input.R.toDouble})")
      println("Ride points missed: " + input.rides.map(_.dist).sum)
      println
      output.score()
    }

  println("Final score: " + finalScores.sum)
}

/**
Score (a_example): 10
Rides missed: 0 (0.0)
Ride points missed: 0

Score (b_should_be_easy): 176877
Rides missed: 6 (0.0075)
Ride points missed: 3621

Score (c_no_hurry): 15790930
Rides missed: 1247 (0.4156666666666667)
Ride points missed: 950043

Score (d_metropolis): 10643137
Rides missed: 4187 (0.4187)
Ride points missed: 3608392

Score (e_high_bonus): 21465945
Rides missed: 16 (0.010666666666666666)
Ride points missed: 12398

Final score: 48076899
 */
