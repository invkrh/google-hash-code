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

  def solve0(in: In, partitionId: Int): Out = {

    val (h, v) = in.photos.partition(_.orientation == "H")

    val hPool = mutable.HashSet() ++ h
    val vPool = mutable.HashSet() ++ v

    val hDict: Map[String, mutable.HashSet[Photo]] = h
      .flatMap(p => p.tags.map(t => (t, p)))
      .groupBy(_._1)
      // Note: never use mapValue. Value will be reevaluated every time.
      .map { case (key, value) => (key, mutable.HashSet() ++ value.map(_._2)) }

    val vDict: Map[String, mutable.HashSet[Photo]] = v
      .flatMap(p => p.tags.map(t => (t, p)))
      .groupBy(_._1)
      // Note: never use mapValue. Value will be reevaluated every time.
      .map { case (key, value) => (key, mutable.HashSet() ++ value.map(_._2)) }

//    println("# H photos = " + hPool.size)
//    println("# H tags = " + hDict.size)
//    println("# H tags per photo = " + hPool.toList.map(_.tags.size).sum / hPool.size.toDouble)
//    println("# H photo per tags = " + hDict.values.map(_.size).sum / hDict.size.toDouble)
//    println("# V photos = " + vPool.size)
//    println("# V tags = " + vDict.size)
//    println("# V tags per photo = " + vPool.toList.map(_.tags.size).sum / vPool.size.toDouble)
//    println("# V photo per tags = " + vDict.values.map(_.size).sum / vDict.size.toDouble)

    val slides = new mutable.ArrayBuffer[Slide]()

    def remove(
        photo: Photo,
        pool: mutable.Set[Photo],
        dict: Map[String, mutable.HashSet[Photo]]): Unit = {

      pool.remove(photo)
      photo.tags foreach { tag =>
        dict(tag).remove(photo)
      }
    }

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

    def bestPhotoScore(
        prevTags: Set[String],
        candidate: Set[Photo],
        chosen: Option[Photo]): Option[(Photo, Int)] = {
      var maxScore = -1
      var bestPhoto: Photo = null
      candidate foreach { photo =>
        val score = chosen match {
          case Some(p) if photo == p => -1 // ignore the chosen one
          case Some(p) => transitionScore(prevTags, photo.tags ++ p.tags)
          case None => transitionScore(prevTags, photo.tags)
        }
        if (score > maxScore) {
          maxScore = score
          bestPhoto = photo
        }
      }
      if (maxScore == -1) {
        None
      } else {
        Some(bestPhoto, maxScore)
      }
    }

    def getBestHorizontalSlide(prevTags: Set[String]): Option[(HorizontalSlide, Int)] = {
      if (hPool.nonEmpty) {
        val candidate = prevTags.flatMap(tag => hDict.getOrElse(tag, Seq()))
        bestPhotoScore(prevTags, candidate, None).map {
          case (photo, score) => (HorizontalSlide(photo), score)
        }
      } else {
        None
      }
    }

    def getBestVerticalSlide(prevTags: Set[String]): Option[(VerticalSlide, Int)] = {
      if (vPool.size >= 2) {
        val candidate = prevTags.flatMap(tag => vDict.getOrElse(tag, Seq()))
        for {
          (left, _) <- bestPhotoScore(prevTags, candidate, None)
          (right, score) <- bestPhotoScore(prevTags, candidate, Some(left))
        } yield {
          (VerticalSlide(left, right), score)
        }
      } else {
        None
      }
    }

    def init(): Slide = {
      // Bootstrap for the first slide
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
