package qualification2015

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.text.SimpleDateFormat
import java.time.LocalDateTime

import scala.io.Source

object Main extends App {

  val prefix = "qualification2015"

  // Data structure

  case class Slot(r: Int, s: Int)
  case class Server(z: Int, c: Int, var ar: Int = -1, var as: Int = -1, var ap: Int = -1) {
    def isAllocated(): Boolean = ar != -1 && as != -1 && ap != -1

    def containsBadSlot(badSlots: List[Slot]): Boolean = {
      if (isAllocated()) {
        (0 until z).map(i => (ar, as + i)).exists(badSlots.contains)
      } else {
        false
      }
    }

    def set(ar: Int, as: Int, ap: Int): this.type = {
      this.ar = ar
      this.as = as
      this.ap = ap
      this
    }

  }
  case class Input(
    R: Int,
    S: Int,
    U: Int,
    P: Int,
    M: Int,
    badSlots: List[Slot],
    servers: List[Server]
  )

  case class Output() {
    override def toString: String =
      input.servers.map { s =>
        if (s.isAllocated()) s"${s.ar} ${s.as} ${s.ap}" else "x"
      } mkString "\n"

    def score(input: Input): Int = {

      // TODO: check slot overlapped

      val valid =
        input.servers.forall(
          s => !s.containsBadSlot(input.badSlots) && s.ar < input.R && s.as < input.S
        ) && input.servers.exists(_.isAllocated())
      if (valid) {
        input.servers.filter(_.isAllocated()).groupBy(_.ap).mapValues(gcPerPool).values.min
      } else 0
    }

    def toFile(path: String): Path = {
      Files.write(Paths.get(path), this.toString.getBytes(StandardCharsets.UTF_8))
    }

    def gcPerPool(pool: List[Server]): Int = {
      val rowCapList =
        pool.groupBy(_.ar).mapValues(serversInRow => serversInRow.map(_.c).sum).values
      rowCapList.sum - rowCapList.max
    }

    def save(): Path = {
      println("Score: " + score(input))
      println(this.toString)
      val timeFormat = new SimpleDateFormat("HH-mm-ss")
      val suffix = timeFormat.format(new java.util.Date())
      this.toFile(s"output/$prefix/heuristic-hao-$suffix.out")
    }
  }

  // Input
  val lines = Source.fromResource(s"$prefix/example.in").getLines
//  val lines = Source.fromResource(s"$prefix/dc.in").getLines
  val Array(r, s, u, p, m) = lines.next().split(" ").map(_.toInt)
  val input = Input(r, s, u, p, m, List.fill(u) {
    val Array(r, s) = lines.next().split(" ").map(_.toInt)
    Slot(r, s)
  }, List.fill(m) {
    val Array(z, c) = lines.next().split(" ").map(_.toInt)
    Server(z, c)
  })

  // Solution

  // Example:
  input.servers(0).set(1, 0, 1)
  input.servers(1).set(1, 3, 0)
  input.servers(2).set(0, 4, 1)
  input.servers(3).set(0, 1, 0)
  input.servers(4)

  // Output
  Output().save()
}
