package org.ml.trees

import scala.collection._
import scala.collection.mutable.ListBuffer

case class TimedObject(name: String, totalTime: Double, executions: List[Double])

case class Started(name: String, time: Long)

case class TimeRecord(name: String, totalTime: Double)

case class TimeReport(records: Array[TimeRecord])

class Timer {

  private val timers = new java.util.concurrent.ConcurrentHashMap[String, mutable.HashMap[Thread, TimedObject]]()

  private val started = new java.util.concurrent.ConcurrentHashMap[String, mutable.HashMap[Thread, Started]]()

  def report(): TimeReport = {
    val out = ListBuffer[TimeRecord]()
    timers.synchronized {
      val keyIt = timers.keySet.iterator()
      while (keyIt.hasNext) {
        val name = keyIt.next()
        val to = timers.get(name)
        val aggTime = to.values.map(_.totalTime).sum
        out += TimeRecord(name, aggTime)
      }
    }
    TimeReport(out.toArray)
  }

  def start(name: String, th: Thread): Unit = {
    started.synchronized {
      if (!started.containsKey(name)) {
        started.put(name, mutable.HashMap[Thread, Started]())
      }
      started.get(name).put(th, Started(name, System.nanoTime()))
    }
  }

  private def addNew[T](mp: java.util.concurrent.ConcurrentHashMap[String, mutable.HashMap[Thread, T]], name: String): Unit = {
    val obj = mp.get(name)
    if (obj == null) {
      mp.synchronized {
        if (!mp.containsKey(name)) {
          mp.put(name, mutable.HashMap[Thread, T]())
        }
      }
    }
  }

  def stop(name: String, th: Thread): Option[TimedObject] = {
    val endTime = System.nanoTime()
    val mp = started.get(name)
    if (mp == null) return None
    mp.get(th) match {
      case Some(startedObj) =>
        mp.remove(th)
        if (mp.isEmpty) {
          started.synchronized {
            val obj = started.get(name)
            if (obj != null && obj.isEmpty) {
              started.remove(name)
            }
          }
        }
        Some(record(startedObj, th, endTime))
      case None => None
    }
  }

  def record(started: Started, th: Thread, endTime: Long): TimedObject = {
    addNew(timers, started.name)
    val mp = timers.get(started.name)
    if (mp == null) {
      //TODO: throw error
    }
    mp.get(th) match {
      case Some(to) =>
        val newTo = toTimedObject(started.name, endTime - started.time)
        mp.put(th, merge(to, newTo))
        newTo
      case None =>
        val to = toTimedObject(started.name, endTime - started.time)
        mp.put(th, to)
        to
    }
  }

  def find(name: String, th: Thread): Option[TimedObject] = {
    val mp = timers.get(name)
    if (mp == null) return None
    mp.get(th)
  }

  def toTimedObject(name: String, diffInNanos: Long): TimedObject = TimedObject(name, inSec(diffInNanos), List(inSec(diffInNanos)))

  implicit def inSec(time: Long): Double = time.toDouble / 1000000000

  def merge(to1: TimedObject, to2: TimedObject): TimedObject =
    TimedObject(to1.name, to1.totalTime + to2.totalTime, List())


}

object Timer {

  private val timer = new Timer()

  def timed[T](name: String = "")(body: => T): T = {
    Timer.start(name)
    val result = body
    Timer.stop(name)
    result
  }

  def start(name: String) = timer.start(name, Thread.currentThread())

  def stop(name: String) = timer.stop(name, Thread.currentThread())

  def provide(name: String): Option[TimedObject] = timer.find(name, Thread.currentThread())

  def report(): TimeReport = timer.report()

}
