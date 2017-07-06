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

import com.colingodsey.stepd.{Math, Pipeline}
import akka.actor._
import akka.util.ByteString
import com.colingodsey.stepd.GCode._

class StepProcessorActor(val next: ActorRef, cfg: PlannerConfig) extends StepProcessor with Pipeline {
  var splits = new Array[Int](4)
  var hasSentSpeed = false

  val ticksPerSecond = cfg.ticksPerSecond
  val stepsPerMM = cfg.stepsPerMM

  var leveling = MeshLevelingReader.Empty

  context.system.eventStream.subscribe(self, classOf[MeshLeveling.Reader])

  def recordSplit(idx: Int): Unit = {
    splits(idx) = splits(idx) + 1

    log debug "split"
  }

  //should move to chunk manager?
  def processChunk(chunk: Array[Byte]): Unit = {
    if(!hasSentSpeed) {
      hasSentSpeed = true

      //sendDown(Raw("C0 S" + ticksPerSecond.toInt))
      sendDown(Raw("C0 S" + ticksPerSecond.toInt))
    }

    sendDown(Chunk(chunk))
  }

  def process(syncPos: StepProcessor.SyncPos): Unit =
    sendDown(syncPos)

  def waitLeveling: Receive = {
    case x: MeshLeveling.Reader =>
      leveling = x
      unstashAll()

      log info "got mesh leveling data"
    case _ => stash()
  }

  def normal: Receive = pipeline orElse {
    case trap: Trapezoid =>
      ack()
      process(trap)
    case x: SetPos =>
      ack()
      setPos(x)
      sendDown(x)
    case ZProbe =>
      ack()
      flushChunk()
      sendDown(ZProbe)

      log info "waiting for leveling data"
      context become waitLeveling
    case cmd: Command if cmd.isGCommand =>
      ack()
      flushChunk()
      sendDown(cmd)
    case cmd: Command =>
      ack()
      sendDown(cmd)
  }

  def receive = normal
}
