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

import com.colingodsey.stepd.GCode._
import com.colingodsey.stepd.planner.StepProcessor.ChunkMeta

class StepProcessorActor(val next: ActorRef, cfg: PlannerConfig) extends StepProcessor(cfg.format)
    with Actor with ActorLogging with Stash {
  var splits = new Array[Int](4)
  var hasSentSpeed = false

  var leveling = MeshLevelingReader.Empty

  val ticksPerSecond = cfg.ticksPerSecond
  val stepsPerMM = cfg.stepsPerMM

  context.system.eventStream.subscribe(self, classOf[MeshLeveling.Reader])

  def recordSplit(idx: Int): Unit = {
    splits(idx) = splits(idx) + 1

    log debug "split"
  }

  //should move to chunk manager?
  def processChunk(chunk: Array[Byte], meta: ChunkMeta): Unit =
    next ! Page(chunk, meta)

  def process(syncPos: StepProcessor.SyncPos): Unit =
    next ! syncPos

  def waitLeveling: Receive = {
    case x: MeshLeveling.Reader =>
      leveling = x
      unstashAll()

      log info "got mesh leveling data"

      context become normal
    case _ => stash()
  }

  def normal: Receive = {
    case trap: Trapezoid =>
      process(trap)

    case x: SetPos =>
      setPos(x)
      next ! x
    case ZProbe =>
      log info "waiting for leveling data"
      context become waitLeveling

      flushChunk()
      next ! ZProbe
    case a @ FlowRate(Some(x)) =>
      flowRate = x / 100.0
      log info s"setting flow rate scale to $flowRate"
      next ! a

    case x: Command if x.isGCommand =>
      flushChunk()
      next ! x
    case x: Command =>
      next ! x
  }

  def receive = normal
}
