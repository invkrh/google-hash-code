package q2019

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.math.min

import util._

object SlideShowSolver extends App {

  case class Photo(id: Int, orientation: String, tags: Set[String]) {
    override def toString: String = orientation + " " + tags.size + " " + tags.mkString(" ")
  }

  sealed trait Slide {
    def tags: Set[String]
  }
  case class HorizontalSlide(photo: Photo) extends Slide {
    override def toString: String = photo.id.toString
    override def tags: Set[String] = photo.tags
  }
  case class VerticalSlide(left: Photo, right: Photo) extends Slide {
    override def toString: String = left.id + " " + right.id
    override def tags: Set[String] = left.tags ++ right.tags
  }

  case class Input(photos: Seq[Photo]) {
    override def toString: String = photos.length + "\n" + photos.mkString("\n")
  }
  case class Output(slides: mutable.ArrayBuffer[Slide]) {
    override def toString: String = slides.length + "\n" + slides.mkString("\n")
    def +(that: Output) = Output(this.slides ++ that.slides)
  }

  var totalScore = 0
  totalScore += new SlideShowSolver(1).run("a_example")
  totalScore += new SlideShowSolver(1).run("b_lovely_landscapes")
  totalScore += new SlideShowSolver(1).run("c_memorable_moments")
  totalScore += new SlideShowSolver(8).run("d_pet_pictures")
  totalScore += new SlideShowSolver(8).run("e_shiny_selfies")
  println(s"Final score = $totalScore")
}

class SlideShowSolver(parallelism: Int, showMetricsBySteps: Boolean = false) extends Solver {
  import SlideShowSolver._

  override type In = Input
  override type Out = Output

  override def round: String = "q2019"

  override def getInput(lines: Iterator[String]): In = {
    val n = lines.next().toInt
    val photos = for (i <- 0 until n) yield {
      val tokens = lines.next().split(" ")
      val orientation = tokens(0)
      val tagSet = tokens.drop(2).toSet
      require(tagSet.size == tokens(1).toInt)
      Photo(i, orientation, tagSet)
    }
    Input(photos)
  }

  private def transitionScore(a: Set[String], b: Set[String]): Int = {
    val left = a.diff(b).size
    val mid = a.intersect(b).size
    val right = b.diff(a).size
    min(min(left, mid), right)
  }

  override def solve(in: In): Out = {
    val numCPUCores = Runtime.getRuntime.availableProcessors()
    val numBatches = parallelism / numCPUCores.toDouble
    println(
      s"# shuffle partitions: $parallelism, # CPU cores: $numCPUCores, " +
        s"# batches: ${math.ceil(numBatches).toInt}")

    /**
     * Hash partition input into  [[parallelism]] blocks
     */
    val partitions = Array.fill(parallelism)(new ArrayBuffer[Photo])
    for {
      photo <- in.photos
    } {
      val rawMod = photo.hashCode() % parallelism
      val partId = rawMod + (if (rawMod < 0) parallelism else 0) // Non negative module
      partitions(partId).append(photo)
    }

    val res = partitions.zipWithIndex.toSeq.par // trigger parallel computing
      .map { case (buffer, id) => solve0(Input(buffer), id) }
      .reduce(_ + _)
    res
  }

  def solve0(block: In, partitionId: Int): Out = {
    def buildTagDict(pool: mutable.HashSet[Photo]): Map[String, mutable.HashSet[Photo]] =
      pool
        .flatMap(p => p.tags.map(t => (t, p)))
        .groupBy(_._1)
        // Note: never use mapValue. Value will be reevaluated every time.
        .map { case (key, value) => (key, mutable.HashSet() ++ value.map(_._2)) }

    // Partition the block into a pool of vertical photos and a pool of horizontal photos
    val (hPool, vPool) = (mutable.HashSet() ++ block.photos).partition(_.orientation == "H")
    // Build the tag dict mapping from tag to a set of vertical and horizontal photos, respectively
    val hDict: Map[String, mutable.HashSet[Photo]] = buildTagDict(hPool)
    val vDict: Map[String, mutable.HashSet[Photo]] = buildTagDict(vPool)

    // Buffer to hold all the slides which will be packaged and returned as the output
    val slides = new mutable.ArrayBuffer[Slide]()

    /**
     * Remove the previously used photo from the corresponding pool and the value set of the tag dict
     * @param photo photo to be removed
     * @param pool the pool holding unused photos
     * @param dict the tag dict of which the value containing unused photos
     */
    def remove(
        photo: Photo,
        pool: mutable.Set[Photo],
        dict: Map[String, mutable.HashSet[Photo]]): Unit = {
      pool.remove(photo)
      photo.tags foreach { tag =>
        dict(tag).remove(photo)
      }
    }

    /**
     * Add the slide to the result and clean up the corresponding pool and tag dict
     * @param slide the slide which should be appended to the result
     */
    def appendToResult(slide: Slide): Unit = {
      slides.append(slide)
      slide match {
        case HorizontalSlide(photo) =>
          remove(photo, hPool, hDict)
        case VerticalSlide(left, right) =>
          remove(left, vPool, vDict)
          remove(right, vPool, vDict)
      }
    }

    /**
     * Find the photo maximizing the score and the max score
     * @param prevTags the tag set of the previous slide
     * @param candidate a set of photos which have at least one common tag with the previous slide
     * @param chosen the first photo used for vertical slide
     */
    def bestPhotoWithScore(
        prevTags: Set[String],
        candidate: Set[Photo],
        chosen: Option[Photo]): Option[(Photo, Int)] = {
      var maxScore = -1
      var bestPhoto: Photo = null
      candidate foreach { p =>
        val score = chosen match {
          case Some(v1) if p == v1 => -1 // ignore the chosen one
          case Some(v1) => transitionScore(prevTags, p.tags ++ v1.tags)
          case None => transitionScore(prevTags, p.tags)
        }
        if (score > maxScore) {
          maxScore = score
          bestPhoto = p
        }
      }
      if (maxScore == -1) {
        None
      } else {
        Some(bestPhoto, maxScore)
      }
    }

    /**
     * Find the best horizontal slide among all the candidate photos containing at least one common tag
     * @param prevTags the tag set of the previous added slide
     */
    def getBestHorizontalSlide(prevTags: Set[String]): Option[(HorizontalSlide, Int)] = {
      if (hPool.nonEmpty) {
        val candidate = prevTags.flatMap(tag => hDict.getOrElse(tag, Seq()))
        bestPhotoWithScore(prevTags, candidate, None).map {
          case (photo, score) => (HorizontalSlide(photo), score)
        }
      } else {
        None
      }
    }

    /**
     * Find the best vertical slide among all the candidate photos containing at least one common tag
     * @param prevTags the tag set of the previous added slide
     */
    def getBestVerticalSlide(prevTags: Set[String]): Option[(VerticalSlide, Int)] = {
      if (vPool.size >= 2) {
        val candidate = prevTags.flatMap(tag => vDict.getOrElse(tag, Seq()))
        for {
          (left, _) <- bestPhotoWithScore(prevTags, candidate, None)
          (right, score) <- bestPhotoWithScore(prevTags, candidate, Some(left))
        } yield {
          (VerticalSlide(left, right), score)
        }
      } else {
        None
      }
    }

    /**
     * Pick a horizontal slide if there are some horizontal photos unused, otherwise pick a
     * vertical slide.
     *
     * Note: this function is only used when
     *     - pick first slide
     *     - pick the next slide when no suitable photos can be found in the candidates
     */
    def init(): Slide = {
      if (hPool.nonEmpty) {
        HorizontalSlide(hPool.maxBy(_.tags.size))
      } else {
        val left = vPool.maxBy(_.tags.size)
        val right = vPool.view.filter(_ != left).maxBy(_.tags.size)
        VerticalSlide(left, right)
      }
    }

    var prevSlide: Slide = init()
    appendToResult(prevSlide)

    // Time statistics
    var prevTime = System.currentTimeMillis()
    var totalTime = 0L
    var cnt = 0L

    while (hPool.size + vPool.size > 1) {
      // Time statistics
      if (showMetricsBySteps && cnt % 100 == 0 && cnt != 0) {
        val curTime = System.currentTimeMillis()
        val delta = curTime - prevTime
        totalTime += delta
        val iteration = cnt / 100
        val averageTime = "%.2f".format(totalTime.toDouble / iteration).toDouble
        println(
          s"[part-$partitionId] # slides added: $cnt, " +
            s"# photos unused: ${hPool.size + vPool.size}, time: $delta ms, " +
            s"averaged: $averageTime ms")
        prevTime = curTime
      }

      prevSlide =
        (getBestHorizontalSlide(prevSlide.tags), getBestVerticalSlide(prevSlide.tags)) match {
          case (Some((hSlide, hScore)), Some((vSlide, vScore))) =>
            if (hScore >= vScore) hSlide else vSlide
          case (Some((hSlide, _)), _) => hSlide
          case (_, Some((vSlide, _))) => vSlide
          case _ => init()
        }
      appendToResult(prevSlide)
      cnt += 1
    }

    Output(slides)
  }

  override def validate(in: In, out: Out): Unit = {
    val res = out.slides.zipWithIndex
      .flatMap {
        case (HorizontalSlide(p), rowNumber) =>
          require(
            p.orientation == "H",
            s"Error: Vertical photos must be used in pairs. Found single vertical photo at line $rowNumber")
          Seq((p.id, rowNumber))
        case (VerticalSlide(left, right), rowNumber) =>
          require(
            left.orientation == "V" && right.orientation == "V",
            s"Error: Only vertical photos can be combined. Found horizontal photo at line $rowNumber")
          Seq((left, rowNumber), (right, rowNumber))
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

  override def computeScore(in: In, out: Out): Int = {
    var prevSlide = out.slides.head
    var sum = 0
    for (curSlide <- out.slides.tail) {
      sum += transitionScore(prevSlide.tags, curSlide.tags)
      prevSlide = curSlide
    }
    sum
  }
}
