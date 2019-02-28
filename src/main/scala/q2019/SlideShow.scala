package q2019

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
 * Step 1: Read statement, define IO interfaces and push (30 mins)
 */
// Input
case class Photo(id: Int, dir: Char, tags: Set[String]) {
  override def toString: String = dir + " " + tags.size + " " + tags.mkString(" ")
  // def toSlide: Slide = if (dir == 'H') Slide(Array(this)) else throw new Exception("vertical")
}
case class Input(photos: Array[Photo]) {
  override def toString: String = photos.size + "\n" + photos.mkString("\n")
}

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
      photos(i) = Photo(i, dir, tokens.drop(2).toSet)
    }
    new Input(photos)
  }
}

// Output
case class Slide(photos: Array[Photo]) {
  override def toString: String = photos.map(_.id).mkString(" ")
  def tags: Set[String] = photos.flatMap(_.tags).toSet
}

object Slide {
  def apply(a: Photo, b: Photo): Slide = {
    Slide(Array(a, b))
  }

  def apply(a: Photo): Slide = {
    Slide(Array(a))
  }
}

case class Output(slides: Array[Slide]) {
  override def toString: String = slides.size + "\n" + slides.map(_.toString).mkString("\n")
  def save(path: String): Path =
    Files.write(Paths.get(path), this.toString.getBytes(StandardCharsets.UTF_8))
}

object SlideShow extends App {
  val basePath = "q2019"

  import math._

  def score(a: Set[String], b: Set[String]) = {
    val left = a.diff(b).size
    val mid = a.intersect(b).size
    val right = b.diff(a).size
    min(min(left, mid), right)
  }

  def pickH(last: Slide, pool: Set[Photo]): Slide = {
    val photo = pool.maxBy(p => score(last.tags, p.tags))
    Slide(photo)
  }

  def pickV(last: Slide, pool: Set[Photo]): Slide = {
    val a = pool.maxBy(p => score(last.tags, p.tags))
    val b = (pool - a).maxBy(p => score(a.tags, p.tags))
    Slide(a, b)
  }

  /**
   * Step 3b: Implement the second solution (should be different) and validate with real data
   */
  def solve(input: Input): Output = {
    val (hor, ver) = input.photos.partition(_.dir == 'H')
    val horSorted = hor.sortBy(-_.tags.size)

    val res = new ArrayBuffer[Slide]()

    def func(last: Slide, hor: Set[Photo], ver: Set[Photo]): Unit = {
      res.append(last)

      if (hor.nonEmpty) {
        val next = pickH(last, hor)
        //println(next.photos.map(_.id).toList)
        func(next, hor - next.photos.head, ver)
      } else if (ver.nonEmpty) {
        val next = pickV(last, ver)
        func(next, hor, ver - next.photos.head - next.photos.last)
      }
    }

    func(Slide(horSorted.head), horSorted.tail.toSet, ver.toSet)

    Output(res.toArray)
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
    // println(input)

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
  // format:off
  val fileList = List(
//    "a_example"
    "b_lovely_landscapes"
//    "c_memorable_moments",
// "d_pet_pictures",
//    "e_shiny_selfies"
  )
  val dataSet = fileList.map(fileName => (fileName, Input(s"$basePath/$fileName.txt"))).toMap
  fileList foreach run
  // format: on
}
