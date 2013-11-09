package com.suguruhamazaki

import akka.actor.{Actor, ActorSystem, Props}
import akka.routing.RoundRobinRouter

object Boot extends App {
  private def calculate(nrOfWorkers: Int, nrOfElements: Int, nrOfMessage: Int) = {
    implicit val system = ActorSystem("pi-calc-system")
    val master = system.actorOf(
      Props(classOf[Master], nrOfWorkers, nrOfMessage, nrOfElements),
      "master")
    master ! Calculate
  }
  calculate(4, 10000, 100000)
}

case class Calculate()
case class Work(start: Int, nrOfElements: Int)
case class Result(value: Double)

trait PiCalculator {
  def calculatePiFor(start: Int, nrOfElements: Int): Double = {
    val range = start * nrOfElements until (start + 1) * nrOfElements
    val calc = (i: Int) ⇒ 4.0 * (1 - (i % 2) * 2) / (2 * i + 1)
    range.foldLeft(0.0)((sum: Double, i: Int) ⇒ sum + calc(i))
  }
}
class Worker extends Actor with PiCalculator {
  def receive: Receive = {
    case Work(start, nrOfElements) ⇒
      val result = calculatePiFor(start, nrOfElements)
      sender ! Result(result)
    case msg ⇒ throw new IllegalArgumentException(s"Unknown message: $msg")
  }
}

class Master(
  nrOfWorkers: Int,
  nrOfMessages: Int,
  nrOfElements: Int) extends Actor {

  var pi: Double = 0.0
  var nrOfResult: Int = 0
  var start: Long = 0
  val workers = context.actorOf(Props(classOf[Worker]).withRouter(
    RoundRobinRouter(nrOfInstances = nrOfWorkers)))

  def receive: Receive = {
    case Calculate ⇒ {
      0 to nrOfMessages foreach { workers ! Work(_,nrOfElements) }
    }
    case Result(value) ⇒ {
      pi += value
      nrOfResult += 1
      if (nrOfResult == nrOfMessages) context.system.shutdown
    }
    case msg ⇒ throw new IllegalArgumentException(s"Unknown message: $msg")
  }

  override def preStart(): Unit = {
    start = System.currentTimeMillis
  }
  override def postStop(): Unit = {
    val duration = System.currentTimeMillis() - start
    println(s"Pi = $pi, time = $duration msec")
  }
}
