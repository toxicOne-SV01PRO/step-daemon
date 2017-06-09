/*
 * Copyright 2017 Colin Godsey
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.colingodsey.stepd.planner

import akka.actor._
import akka.util.ByteString
import com.colingodsey.stepd.CommandParser._
import com.colingodsey.stepd.{LineSerial, Parser}
import com.colingodsey.stepd.planner.Math.{Accel, Jerk, MathFault, Position}
import com.colingodsey.stepd.Parser.{CMove, GCodeCommand}

import scala.concurrent.duration._

object Pipeline {
  case object Ack

  val MaxPending = 32
  val MinPending = 16
}

trait Pipeline extends Actor with Stash with ActorLogging {
  import Pipeline._

  var pending = 0
  var isWaiting = false

  def next: ActorRef

  def sendDown(msg: Any): Unit = {
    next ! msg
    pending += 1

    if(pending >= MaxPending && !isWaiting) {
      context.become(waiting, discardOld = false)
      isWaiting = true
    }
  }

  def ack(ref: ActorRef = sender): Unit = ref ! Ack

  def woke(): Unit = {}

  def pipeline: Receive = {
    case Ack =>
      pending -= 1
  }

  def waiting: Receive = {
    case Ack =>
      pending -= 1

      require(pending >= 0, "Ack tracking fault! Is something double Acking somewhere?")

      if(pending < MinPending) {
        context.unbecome()
        isWaiting = false
        unstashAll()
        woke()
      }
    case _ => stash()
  }
}

object M114Handler {
  case class Done(pos: Position)
}

//this goofy class exists just so we can get position information synced back to the pipeline
class M114Handler(deltaProcessor: ActorRef, lineSerial: ActorRef) extends Actor {
  lineSerial ! LineSerial.Subscribe

  def receive: Receive = {
    case LineSerial.Response(str) if str.startsWith("X:") && str.contains(" Count ") =>
      //Recv: X:0.00 Y:0.00 Z:10.00 E:0.00 Count X:0 Y:0 Z:16000
      val parts = str.trim.split(' ')

      val x = parts(0).drop(2).toFloat
      val y = parts(1).drop(2).toFloat
      val z = parts(2).drop(2).toFloat
      val e = parts(3).drop(2).toFloat

      deltaProcessor ! M114Handler.Done(Position(x, y, z, e))
  }
}

class DeltaProcessorActor(val next: ActorRef, ignoreM114: Boolean) extends DeltaProcessor with Pipeline {
  import context.dispatcher

  var deltasProcessed: Int = 0
  var curTimer: Option[Cancellable] = None

  //TODO: maybe need a timeout here?
  def waitingM114: Receive = {
    case M114Handler.Done(newPos) =>
      pos = newPos

      sendDown(getSetPos)

      unstashAll()
      context become receive
    case _ => stash()
  }

  def checkPosTimer(): Unit = if(curTimer == None) {
    curTimer = Some(context.system.scheduler.scheduleOnce(1.second, self, PosTimer))
  }

  def receive: Receive = pipeline orElse {
    case M114 if !ignoreM114 =>
      //stop processing all other messages until we get a response from this
      ack()
      sendDown(M114)
      context become waitingM114

      log info "syncing pipeline position"
    case x: SetPos =>
      ack()
      processSetPos(x)
      //sendDown(getSetPos)
      sendDown(x)
    case x: GMove =>
      ack()
      processGMove(x)
    case x: GCodeCommand =>
      ack()

      //sendDown(getSetPos)
      sendDown(x)
    case PosTimer =>
      curTimer = None

      sendDown(getSetPos)
  }

  def processMoveDelta(delta: MoveDelta): Unit = {
    sendDown(delta)

    deltasProcessed += 1
  }

  object PosTimer
}

class PhysicsProcessorActor(val next: ActorRef, val acc: Accel, val jerk: Jerk) extends PhysicsProcessor with Pipeline {
  var faultCounts = Map[MathFault, Int]()

  def recordFault(fault: MathFault): Unit = {
    val count = faultCounts.getOrElse(fault, 0) + 1

    faultCounts += fault -> count

    log.warning("fault: {}", fault.toString)
  }

  def processTrapezoid(trap: Trapezoid): Unit = {
    sendDown(trap)
  }

  def endTrapAndContinue(cmd: Any): Unit = {
    ack()

    //send an empty move so we can finish the pending trap, maintain linearization
    flush()
    sendDown(cmd)
  }

  def receive: Receive = pipeline orElse {
    case delta: MoveDelta if delta.isEOrZOnly =>
      //TODO: this needed?
      ack()
      flush()
      processDelta(delta)
      flush()
    case delta: MoveDelta =>
      ack()
      processDelta(delta)
    case cmd: SetPos =>
      endTrapAndContinue(cmd)
    case cmd: GCodeCommand =>
      endTrapAndContinue(cmd)
  }
}

class StepProcessorActor(val next: ActorRef, val steps: StepProcessor.StepSettings, val leveling: MeshLeveling.Reader) extends StepProcessor with Pipeline {
  var splits = new Array[Int](4)
  var hasSentSpeed = false

  def recordSplit(idx: Int): Unit = {
    splits(idx) = splits(idx) + 1

    log.debug("split")
  }

  def processChunk(chunk: ByteString): Unit = {
    if(!hasSentSpeed) {
      hasSentSpeed = true

      sendDown(Parser.Raw("C0 S" + steps.ticksPerSecond.toInt))
    }

    sendDown(chunk)
  }

  def receive: Receive = pipeline orElse {
    case trap: Trapezoid =>
      ack()
      processTrap(trap)
    case x: SetPos =>
      ack()
      setPos(x)
      sendDown(x)
    case cmd: GCodeCommand if cmd.isGCommand =>
      ack()
      flushChunk()
      sendDown(cmd)
    case cmd: GCodeCommand =>
      ack()
      sendDown(cmd)
  }
}

