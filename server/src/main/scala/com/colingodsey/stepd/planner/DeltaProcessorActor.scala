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

      sendDown(SetPos(pos))

      unstashAll()
      context become receive
    case _ => stash()
  }

  def receive: Receive = pipeline orElse {
    case GetPos if !ignoreM114 =>
      //stop processing all other messages until we get a response from this
      ack()
      sendDown(GetPos)
      context become waitingM114

      log info "syncing pipeline position"
    case x: SetPos =>
      ack()
      process(x)
      //sendDown(getSetPos)
      sendDown(x)
    case x: GMove =>
      ack()
      process(x)
    case x: Command =>
      ack()

      //sendDown(getSetPos)
      sendDown(x)
  }

  def process(delta: MoveDelta): Unit = {
    sendDown(delta)

    deltasProcessed += 1
  }

  override def preStart(): Unit = {
    super.preStart()
    //checkPosTimer()
  }

  override def postStop(): Unit = {
    curTimer.foreach(_.cancel())
  }
}
