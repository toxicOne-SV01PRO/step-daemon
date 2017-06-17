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

import com.colingodsey.stepd.Math._

/* takes move deltas, and produces iterable positions */
trait PhysicsProcessor {
  def acc: Vector4D
  def jerk: Vector4D

  var lastDelta = MoveDelta.Empty
  var curDelta = MoveDelta.Empty

  def recordFault(fault: MathFault): Unit

  def processTrapezoid(trap: Trapezoid): Unit

  def maxResizes = 15

  def popDelta(nextDelta: MoveDelta): Unit = {
    lastDelta = curDelta
    curDelta = nextDelta
  }

  def flush(): Unit = {
    processDelta(MoveDelta.Empty)
    processDelta(MoveDelta.Empty)
    processDelta(MoveDelta.Empty)
  }

  def willStartFrCauseResize(startFr: Double, post: MoveDelta): Boolean = {
    val dfr = post.f - startFr
    val accel = post.d.abs.normal * acc

    val accelTime = if(accel == 0) 0.0 else dfr / accel
    val accelDist = accel * accelTime * accelTime * 0.5 + startFr * accelTime

    post.isValid && accelDist >= (post.length * 0.5)
  }

  //TODO: is there any way to do a resize that allows a start *deccel* instead of accel?

  /*
  Use the inner (dot) product of the 4d vectors to determine jerk, accel, and junction feed rate.

  For the junction fr, the dot product of the 2 movement vectors is taken, and clamped to [0, 1].
  The will produce a junction of 0 for any angles that are 90* or more.

  Jerk is calculated by setting a floor based on the dot product of the change in velocity vectors,
  if below this floor, the junction fr is 100% of the smaller of either fr (no accel).

  Acceleration is calculated as the dot product of the movement vector (normalized absolute)
  and the acceleration vector. Because both of these have positive-only values for each dimension,
  the dot product produced is between 0 and acc.length. Should never be 0 for real values.

  Invalid pre or post moves force a junction fr of 0.
   */
  def processTrapezoid(pre: MoveDelta, moveDelta: MoveDelta, post: MoveDelta): Unit = {
    val dvStart = moveDelta.v - pre.v
    val frMaxStart = math.min(moveDelta.f, pre.f)
    val frStart = if(pre.isValid) frMaxStart * {
      val f = pre.d.normal * moveDelta.d.normal

      if(dvStart.abs * jerk.normal < jerk.length) 1.0
      else clamp(0.0, f, 1.0)
    } else 0.0

    val frAccel = moveDelta.d.abs.normal * acc

    val dvEnd = post.v - moveDelta.v
    val frMaxEnd = math.min(moveDelta.f, post.f)
    val frEnd = if(post.isValid) frMaxEnd * {
      val f = moveDelta.d.normal * post.d.normal

      if(dvEnd.abs * jerk.normal < jerk.length) 1.0
      else clamp(0.0, f, 1.0)
    } else 0.0

    val frDeccel = -frAccel

    if(willStartFrCauseResize(frEnd, post)) throw LookaheadFault

    require(frAccel >= 0)
    require(frDeccel <= 0)

    val trap = Trapezoid(frStart, frAccel, moveDelta, frDeccel, frEnd)

    processTrapezoid(trap)
  }

  def processTrapezoidSafe(pre: MoveDelta, moveDelta: MoveDelta, post: MoveDelta, maxTimes: Int = maxResizes): Unit = {
    if(!moveDelta.isValid) return

    try processTrapezoid(pre, moveDelta, post) catch {
      case x: EaseLimit if maxTimes == 0 =>
        sys.error("Failed reducing trapezoid for acceleration")
        recordFault(x)
      case x: EaseLimit =>
        val newDelta = moveDelta.scaleFr(0.5)

        if(maxTimes == maxResizes) recordFault(x)
        processTrapezoidSafe(pre, newDelta, post, maxTimes - 1)
    }
  }

  def processDelta(nextDelta: MoveDelta): Unit = {
    try {
      processTrapezoidSafe(lastDelta, curDelta, nextDelta)
      popDelta(nextDelta)
    } catch {
      case LookaheadFault =>
        recordFault(LookaheadFault)

        require(nextDelta.f > 0.001, "unable to handle LookaheadHalt")

        processDelta(nextDelta.scaleFr(0.5))
    }
  }
}
