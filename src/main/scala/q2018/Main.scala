package q2018

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.text.SimpleDateFormat

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

object Main extends App {

  import scala.math._
  // Data structure
  case class Ride(
    id: Int,
    startX: Int,
    startY: Int,
    endX: Int,
    endY: Int,
    startTime: Int,
    endTime: Int
  ) {
    val dist: Int = abs(endX - startX) + abs(endY - startY)
  }
  case class Input(R: Int, C: Int, F: Int, N: Int, B: Int, T: Int, rides: ListBuffer[Ride])
  object Input {
    def apply(path: String): Input = {
      val lines = Source.fromResource(path).getLines
      val Array(r, c, f, n, b, t) = lines.next().split(" ").map(_.toInt)
      val rides = ListBuffer.tabulate(n) { i =>
        val Array(a, b, x, y, s, f) = lines.next().split(" ").map(_.toInt)
        Ride(i, a, b, x, y, s, f)
      }
      Input(r, c, f, n, b, t, rides)
    }
  }

  trait Motivation extends ((Vehicle, Ride, Int, Int) => Double)

  object Zero extends Motivation {
    override def apply(v: Vehicle, r: Ride, B: Int, t: Int): Double = {
      0d
    }
  }

  object Ratio extends Motivation {
    override def apply(v: Vehicle, r: Ride, B: Int, t: Int): Double = {
      val distToStart = v.dist2Start(r)
      val bonus = if (t + distToStart <= r.startTime) B else 0
      val value = r.dist + bonus
      val cost = if (t + distToStart <= r.startTime) {
        r.startTime - t // general case
      } else {
        distToStart
      }
      value / cost.toDouble
    }
  }

  object Loss extends Motivation {
    override def apply(v: Vehicle, r: Ride, B: Int, t: Int): Double = {
      val distToStart = v.dist2Start(r)
      val waitTime = max(r.startTime - (t + distToStart), 0)
      val bonus = if (t + distToStart <= r.startTime) B else 0

      val value = r.dist + bonus
      val cost = if (t + distToStart <= r.startTime) {
        r.startTime - t
      } else {
        distToStart
      }
      bonus - distToStart - waitTime
    }
  }

  case class Vehicle(
    var posX: Int = 0,
    var posY: Int = 0,
    ridesTaken: ArrayBuffer[Ride] = new ArrayBuffer()
  )(implicit context: Input) {

    var currentRide: Option[Ride] = None
    var currentRideEndTime = 0
    var motivation: Motivation = Zero

    def withMotivation(m: Motivation): this.type = {
      this.motivation = m
      this
    }

    def dist2Start(r: Ride): Int = {
      Math.abs(posX - r.startX) + Math.abs(posY - r.startY)
    }

    def bestRide(t: Int): (Ride, Double) = {
      var bestROI = Double.NegativeInfinity
      var bestRide = context.rides.head
      for (r <- context.rides) {
        val roi =
          if (finishTime(t, r) > r.endTime) Double.NegativeInfinity
          else {
            motivation(this, r, context.B, t)
          }
        if (roi > bestROI) {
          bestROI = roi
          bestRide = r
        }
      }
      (bestRide, bestROI)
    }

    def selectRide(t: Int): Unit = {
      if (context.rides.nonEmpty) {
        val (ride, roi) = bestRide(t)
        if (roi != Double.NegativeInfinity) {
          context.rides -= ride
          this.ridesTaken.append(ride)
          this.currentRide = Some(ride)
          this.currentRideEndTime = finishTime(t, ride)
        }
      }
    }

    def finishTime(t: Int, r: Ride): Int = {
      val timeToPick = dist2Start(r)
      if (t + timeToPick <= r.startTime) {
        r.dist + r.startTime
      } else {
        t + r.dist + timeToPick
      }
    }

    def bonus(t: Int, r: Ride): Int = {
      if (t + this.dist2Start(r) <= r.startTime) context.B else 0
    }

    def update(t: Int): Unit = {
      currentRide match {
        case Some(ride) =>
          if (t == currentRideEndTime) {
            currentRide = None
            this.posX = ride.endX
            this.posY = ride.endY
            selectRide(t)
          }
        case None => selectRide(t) // Init
      }
    }
  }

  case class Output(problem: String)(implicit context: Input) {
    val vehicles: Array[Vehicle] = Array.fill(context.F)(Vehicle()(context))

    def solve(m: Motivation): Int = {
      val start = System.currentTimeMillis()
      for {
        t <- 0 until context.T // if { println(t); true }
        v <- vehicles
      } {
        v.withMotivation(m).update(t)
      }
      val solutionTime = (System.currentTimeMillis() - start) / 1000d

      val s = score()
      println(s"Score ($problem): " + score())
      println("Ride points missed: " + context.rides.map(_.dist).sum)
      println(s"Solution time: $solutionTime seconds")
      println
      s
    }

    override def toString: String =
      vehicles
        .map { v =>
          v.ridesTaken.size + " " + v.ridesTaken.map(_.id).mkString(" ")
        }
        .mkString("\n")

    def score(): Int = {
      vehicles.map { v =>
        val dummyVehicles = Vehicle()
        val (_, _, score) = v.ridesTaken.foldLeft((dummyVehicles, 0, 0)) {
          case ((dv, t, s), r) =>
            val bonus = dv.bonus(t, r)
            val endTime = dv.finishTime(t, r)
            (Vehicle(r.endX, r.endY), endTime, s + r.dist + bonus)
        }
        score
      }.sum
    }

    def save(): Path = {
      val timeFormat = new SimpleDateFormat("HH-mm-ss")
      val suffix = timeFormat.format(new java.util.Date())
      val path = s"output/$prefix/$problem-hao-$suffix.out"
      Files.write(Paths.get(path), this.toString.getBytes(StandardCharsets.UTF_8))
    }
  }

  val prefix = "q2018"

  val settings = List(
    "a_example" -> Loss,
    "b_should_be_easy" -> Loss,
    "c_no_hurry" -> Loss,
    "d_metropolis" -> Loss,
    "e_high_bonus" -> Loss
  )

  val finalScore = settings.map {
    case (problem, motivation) =>
      implicit val input: Input = Input(s"$prefix/$problem.in")
      val output: Output = Output(problem)
      val score = output.solve(motivation)
      output.save()
      score
  }.sum

  println("Final score: " + finalScore)
}

/**

Score (a_example): 10
Ride points missed: 0
Solution time: 0.03 seconds

Score (b_should_be_easy): 176877
Ride points missed: 3621
Solution time: 0.282 seconds

Score (c_no_hurry): 15790161
Ride points missed: 950812
Solution time: 3.121 seconds

Score (d_metropolis): 11760173
Ride points missed: 2496873
Solution time: 16.777 seconds

Score (e_high_bonus): 21465945
Ride points missed: 12398
Solution time: 3.164 seconds

Final score: 49193166

 */
