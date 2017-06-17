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
import akka.actor._
import com.colingodsey.stepd.GCode._
import com.colingodsey.stepd.Math._

import scala.concurrent.duration._

class PhysicsProcessorActor(val next: ActorRef, cfg: PlannerConfig) extends PhysicsProcessor with Pipeline {
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

  def acc = cfg.accel
  def jerk = cfg.jerk

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
