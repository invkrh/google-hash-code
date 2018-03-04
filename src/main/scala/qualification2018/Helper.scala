package qualification2018

object Helper {
  def probe(f: => Unit): Unit = {
    val start = System.currentTimeMillis()
    f
    println("time: " + (System.currentTimeMillis() - start))
  }
}
