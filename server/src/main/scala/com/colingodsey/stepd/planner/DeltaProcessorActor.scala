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

import com.colingodsey.stepd.Pipeline
import com.colingodsey.stepd.GCode._
import akka.actor._
import scala.concurrent.duration._

class DeltaProcessorActor(val next: ActorRef, ignoreM114: Boolean) extends DeltaProcessor with Pipeline {
  import context.dispatcher

  var deltasProcessed: Int = 0
  var curTimer: Option[Cancellable] = None

  //TODO: maybe need a timeout here?
  def waitingM114: Receive = {
    case PipeSyncHandler.Done(newPos) =>
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

  override def postStop(): Unit = {
    curTimer.foreach(_.cancel())
  }

  object PosTimer
}
