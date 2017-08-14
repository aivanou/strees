
import java.util.concurrent.Executors

import core.exec._

import scala.collection._

object Main {
  def main(args: Array[String]): Unit = {
    val ranges = Array[(Int, Int)]((0, 10), (11, 20), (21, 30), (31, 40), (41, 50))
    val mp = mutable.HashMap[Int, Int]()
    val exec = Executors.newFixedThreadPool(6)
    for (range <- ranges) {
      val w = new W(range._1, range._2, mp)
      exec.submit(w)
    }
    Thread.sleep(1000)
  }

  class W(f: Int, t: Int, mp: mutable.HashMap[Int, Int]) extends Runnable {
    val rng = new java.util.Random(System.nanoTime())

    override def run(): Unit = {
      while (true) {
        val k = rng.nextInt(t - f) + f
        val v = rng.nextInt(t - f) + f
        mp.put(k, v)
      }
    }
  }

}
