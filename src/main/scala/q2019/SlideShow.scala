package q2019

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import scala.collection.mutable
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
  val rng = new Random(19890118)

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

    def takeDistinct(
        tags: Set[String],
        cache: Map[String, Array[Int]],
        n: Int): mutable.HashSet[Int] = {
      val res = new mutable.HashSet[Int]()
      for {
        tag <- rng.shuffle(tags.toSeq) if res.size < n
        photoIds <- cache
          .get(tag)
          .map(ids => rng.shuffle(ids.filter(id => !used(id)).toSeq)) // in case some tags exist in one cache not in the other
      } {
        for {
          id <- photoIds if res.size < n
        } {
          res.add(id)
        }
      }
      res
    }

    // TODO: use foldLeft to avoid var and make parallel operation deterministic
    def pickH(tags: Set[String], candidate: mutable.HashSet[Int]): Option[(Array[Int], Int)] = {
      var maxId = -1
      var maxScore = 0
      for (photoId <- candidate.par) {
        val curScore = score(input.photos(photoId).tags, tags)
        if (curScore > maxScore) {
          maxScore = curScore
          maxId = photoId
        }
      }
      if (maxId == -1) None else Some((Array(maxId), maxScore))
    }

    def pickV(tags: Set[String], candidate: mutable.HashSet[Int]): Option[(Array[Int], Int)] = {
      var maxId = -1
      var maxScore = 0

      var subMaxId = -1
      var subMaxScore = 0

      for (photoId <- candidate.par) {
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
    var totalTime = 0L
    var iteration = 0L
    def rec(slide: Array[Int]): Unit = {
      val curTime = System.currentTimeMillis()
      if (buff.size % 100 == 0) {
        val delta = curTime - prevTime
        totalTime += delta
        iteration += 1
        val averageTime = "%.2f".format(totalTime.toDouble / iteration).toDouble
        println(
          s"Slideshow size = ${buff.size}, " +
            s"iteration time = $delta ms, " +
            s"average time = $averageTime ms")
        prevTime = curTime
      }

      buff.append(slide)
      slide.foreach(id => used(id) = true)
      val tags = slide.flatMap(id => input.photos(id).tags).toSet

      val maxCandidate = 10000

      val candidatesH = takeDistinct(tags, cacheH, maxCandidate)
      val candidatesV = takeDistinct(tags, cacheV, maxCandidate)

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

  def validate(input: Input, output: Output): Unit = {
    val res = output.slides.zipWithIndex
      .flatMap {
        case (slide, rowNumber) =>
          slide.length match {
            case 0 => throw new Exception(s"Error: Line $rowNumber is empty")
            case 1 if input.photos(slide.head).dir == 'V' =>
              throw new Exception(
                s"Error: Vertical photos must be used in pairs. Found single vertical photo at line $rowNumber")
            case 2 if slide.exists(id => input.photos(id).dir == 'H') =>
              throw new Exception(
                s"Error: Only vertical photos can be combined. Found horizontal photo at line $rowNumber")
            case i if i > 2 =>
              throw new Exception(
                s"Error: Too many photos: found $i, but should be between 1 and 2 at line $rowNumber")
            case _ => slide.map(pId => (pId, rowNumber))
          }
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

  // TODO: use foldLeft to avoid var
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
  val finalScore = List(
//    "a_example",
//    "b_lovely_landscapes",
//    "c_memorable_moments",
    "d_pet_pictures"
//    "e_shiny_selfies"
  ).map(run).sum
  // format: on
  // println(s"Final Score = $finalScore")
}
