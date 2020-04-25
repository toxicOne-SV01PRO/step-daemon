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

package com.colingodsey.stepd

import akka.actor._

object Pipeline {
  case object Ack
  case object Status

  val MaxPending = 32
  val MinPending = 24

  trait Terminator { _: Actor =>
    def ack(ref: ActorRef = sender): Unit = ref ! Ack
  }
}

trait Pipeline extends Actor with Stash with ActorLogging with Pipeline.Terminator {
  import Pipeline._

  var pending = 0
  var isWaiting = false

  import scala.concurrent.duration._
  implicit def __ec = context.dispatcher
  context.system.scheduler.scheduleWithFixedDelay(1.seconds, 5.seconds) { () =>
    //TEST!!!!
    log.info(s"isWaiting $isWaiting")
  }

  def next: ActorRef

  def maxPending = Pipeline.MaxPending
  def minPending = Pipeline.MinPending

  def sendDown(msg: Any): Unit = {
    next ! msg
    pending += 1

    if(pending >= maxPending && !isWaiting) {
      context.become(waiting, discardOld = false)
      isWaiting = true
      onWait()
    }
  }

  def onWake(): Unit = log debug "onWake"

  def onWait(): Unit = log debug "onWait"

  def pipeline: Receive = {
    case Ack =>
      pending -= 1
  }

  def waiting: Receive = {
    case Ack =>
      pending -= 1

      require(pending >= 0, "Ack tracking fault! Is something double Acking somewhere?")

      if(pending < minPending) {
        context.unbecome()
        isWaiting = false
        unstashAll()
        onWake()
      }
    case Status =>
    case _ => stash()
  }
}

