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

  val MaxPending = 32
  val MinPending = 16

  trait Terminator { _: Actor =>
    def ack(ref: ActorRef = sender): Unit = ref ! Ack
  }
}

trait Pipeline extends Actor with Stash with ActorLogging with Pipeline.Terminator {
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
      onWait()
    }
  }

  def onWake(): Unit = log info "onWake"

  def onWait(): Unit = log info "onWait"

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
        onWake()
      }
    case _ => stash()
  }
}

