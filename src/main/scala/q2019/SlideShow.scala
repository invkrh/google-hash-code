package q2019

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Random

// Input
case class Photo(id: Int, dir: Char, tags: Set[String]) {
  override def toString: String = dir + " " + tags.size + " " + tags.mkString(" ")
}
case class Input(photos: Array[Photo]) {
  override def toString: String = photos.size + "\n" + photos.mkString("\n")
}

object Input {
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
case class Output(slides: Array[Array[Int]]) {
  override def toString: String = slides.length + "\n" + slides.map(_.mkString(" ")).mkString("\n")
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

  def time[T](action: => T): T = {
    val start = System.currentTimeMillis()
    val res = action
    println("Time = " + (System.currentTimeMillis() - start) + " ms")
    res
  }

  def solve(input: Input): Output = {
    val (h, v) = input.photos.partition(_.dir == 'H')
    println("Horizontal photos: " + h.length)
    println("Vertical photos: " + v.length)
    val cacheH = h.flatMap(p => p.tags.map(t => (t, p.id))).groupBy(_._1).mapValues(_.map(_._2))
    val cacheV = v.flatMap(p => p.tags.map(t => (t, p.id))).groupBy(_._1).mapValues(_.map(_._2))

    val buff = new ArrayBuffer[Array[Int]]()
    val used = new Array[Boolean](input.photos.length)

    def pickH(tags: Set[String], candidate: Traversable[Int]): Option[(Array[Int], Int)] = {
      var maxId = -1
      var maxScore = 0
      for (photoId <- candidate) {
        val curScore = score(input.photos(photoId).tags, tags)
        if (curScore > maxScore) {
          maxScore = curScore
          maxId = photoId
        }
      }
      if (maxId == -1) None else Some((Array(maxId), maxScore))
    }

    def pickV(tags: Set[String], candidate: Traversable[Int]): Option[(Array[Int], Int)] = {
      var maxId = -1
      var maxScore = 0

      var subMaxId = -1
      var subMaxScore = 0

      for (photoId <- candidate if photoId != maxId && photoId != subMaxId) {
        val curScore = score(input.photos(photoId).tags, tags)
        if (curScore > maxScore) {
          subMaxId = maxId
          subMaxScore = maxScore
          maxId = photoId
          maxScore = curScore
        } else if (curScore > subMaxId) {
          subMaxId = photoId
          subMaxScore = curScore
        }
      }

      if (maxId == -1 || subMaxId == -1) {
        None
      } else {
        val slide = Array(maxId, subMaxId)
        val slideScore = score(slide.flatMap(id => input.photos(id).tags).toSet, tags)
        Some((slide, slideScore))
      }
    }

    var prevTime = System.currentTimeMillis()
    def rec(slide: Array[Int]): Unit = {
      val curTime = System.currentTimeMillis()
      if (buff.size % 100 == 0) {
        println(
          "buffer size = " + buff.size + ", time for 100 slides = " + (curTime - prevTime) + "ms")
        prevTime = curTime
      }

      buff.append(slide)
      slide.foreach(id => used(id) = true)
      val tags = slide.flatMap(id => input.photos(id).tags).toSet

      val maxCandidate = 2500

      val candidatesH: Traversable[Int] =
        tags.view
          .flatMap { tag =>
            cacheH.get(tag).map(ids => ids.filter(id => !used(id)))
          }
          .flatten
          .take(maxCandidate)

      val candidatesV: Traversable[Int] =
        tags.view
          .flatMap { tag =>
            cacheV.get(tag).map(ids => ids.filter(id => !used(id)))
          }
          .flatten
          .take(maxCandidate)

      (pickH(tags, candidatesH), pickV(tags, candidatesV)) match {
        case (Some((hSlide, hScore)), Some((vSlide, vScore))) =>
          if (hScore >= vScore) {
            rec(hSlide)
          } else {
            rec(vSlide)
          }
        case (_, Some((vSlide, _))) => rec(vSlide)
        case (Some((hSlide, _)), _) => rec(hSlide)
        case _ =>
      }
    }

    val rng = new Random(19910903)
    val init =
      if (h.nonEmpty && v.nonEmpty) {
        if (rng.nextBoolean()) {
          Array(h(rng.nextInt(h.length)).id)
        } else {
          Array(v(rng.nextInt(v.length / 2)).id, v(v.length / 2 + rng.nextInt(v.length / 2)).id)
        }
      } else if (h.nonEmpty) {
        Array(h(rng.nextInt(h.length)).id)
      } else {
        Array(v(rng.nextInt(v.length / 2)).id, v(v.length / 2 + rng.nextInt(v.length / 2)).id)
      }

    rec(init)
    for (photo <- input.photos) {
      if (!used(photo.id)) {
        if (photo.dir == 'H') {
          rec(Array(photo.id))
        } else {
          val theOtherId = input.photos.indexWhere(p => p.dir == 'V' && !used(p.id), photo.id + 1)
          if (theOtherId > 0) {
            rec(Array(photo.id, theOtherId))
          }
        }
      }
    }

    Output(buff.toArray)
  }

  /**
   *  TODO
   */
  def validate(input: Input, output: Output): Unit = {
    val res = output.slides.zipWithIndex
      .flatMap {
        case (photos, rowNumber) => photos.map(pId => (pId, rowNumber))
      }
      .groupBy(_._1)
      .mapValues(_.map(_._2))

    for {
      (pid, rows) <- res
    } {
      if (rows.length > 1) {
        throw new Exception(s"Error: Photo $pid used at lines ${rows.mkString("[", ",", "]")}")
      }
    }
  }

  def computeScore(input: Input, output: Output): Int = {
    var prev = output.slides.head
    var sum = 0
    for (slide <- output.slides.tail) {
      sum += score(
        prev.flatMap(id => input.photos(id).tags).toSet,
        slide.flatMap(id => input.photos(id).tags).toSet,
      )
      prev = slide
    }
    sum
  }

  def run(fileName: String): Int = {

    val input = Input(s"$basePath/$fileName.txt")
    val output = time { solve(input) }

    validate(input, output)

    val score = computeScore(input, output)
    println(s"Score($fileName) = " + score)
    output.save(s"output/$basePath/$fileName.out")
    score
  }

  /**
   * Step 4: Upload output files to gain points
   */
  // format: off
  val fileList = List(
//    "a_example"
//    "b_lovely_landscapes"
//    "c_memorable_moments"
    "d_pet_pictures"
//    "e_shiny_selfies"
  ) foreach run
  // format: on
}
