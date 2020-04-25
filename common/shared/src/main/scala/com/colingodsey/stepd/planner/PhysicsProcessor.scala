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
object PhysicsProcessor {
  final val MaxResizes = 30
  final val ResizeFactor = 0.80
}

trait PhysicsProcessor {
  import PhysicsProcessor._

  var lastDelta = MoveDelta.Empty
  var curDelta = MoveDelta.Empty

  def acc: Vec4
  def jerk: Vec4

  def recordFault(fault: MathFault): Unit

  def process(trap: Trapezoid): Unit

  def maxResizes = MaxResizes

  def pushDelta(nextDelta: MoveDelta): Unit = {
    lastDelta = curDelta
    curDelta = nextDelta
  }

  def flushDelta(): Unit = {
    process(MoveDelta.Empty)
    process(MoveDelta.Empty)
    process(MoveDelta.Empty)
  }

  def willStartFrCauseResize(startFr: Double, post: MoveDelta): Boolean = {
    val dfr = post.f - startFr
    val accel = post.d.abs.normal ⋅ acc

    val accelTime = if(accel == 0) 0.0 else dfr / accel
    val accelDist = accel * accelTime * accelTime * 0.5 + startFr * accelTime

    //comparing to the half point is inaccurate, but we dont know the next trap
    post.isValid && accelDist >= (post.length * 0.5)
  }

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
  def createTrapezoid(pre: MoveDelta, moveDelta: MoveDelta, post: MoveDelta): Unit = {
    val dvStart = moveDelta.v - pre.v
    val frMaxStart = math.min(moveDelta.f, pre.f)
    val frStart = if(pre.isValid) frMaxStart * {
      val f = pre.d.normal ⋅ moveDelta.d.normal
      val jf = dvStart.abs ⋅ jerk.normal

      //TODO: use jerk factor as lower bounds for clamp? scaled for the fr...

      if(jf < jerk.length) 1.0
      else clamp(0.0, f, 1.0)
    } else 0.0

    val frAccel = moveDelta.d.abs.normal ⋅ acc

    val dvEnd = post.v - moveDelta.v
    val frMaxEnd = math.min(moveDelta.f, post.f)
    val frEnd = if(post.isValid) frMaxEnd * {
      val f = moveDelta.d.normal ⋅ post.d.normal
      val jf = dvEnd.abs ⋅ jerk.normal

      if(jf < jerk.length) 1.0
      else clamp(0.0, f, 1.0)
    } else 0.0

    val frDeccel = -frAccel

    if(willStartFrCauseResize(frEnd, post)) throw LookaheadFault

    require(frAccel >= 0)
    require(frDeccel <= 0)

    val trap = Trapezoid(frStart, frAccel, moveDelta, frDeccel, frEnd)

    process(trap)
  }

  def createTrapezoidSafe(pre: MoveDelta, moveDelta: MoveDelta, post: MoveDelta, maxTimes: Int = maxResizes): Unit = {
    if(!moveDelta.isValid) return

    try createTrapezoid(pre, moveDelta, post) catch {
      case x: EaseLimit if maxTimes == 0 =>
        sys.error("Failed reducing trapezoid for acceleration")
        recordFault(x)
      case x: EaseLimit =>
        if(maxTimes == maxResizes) recordFault(x)

        createTrapezoidSafe(pre, moveDelta scaleFr ResizeFactor, post, maxTimes - 1)
    }
  }

  def process(nextDelta: MoveDelta): Unit = {
    try {
      createTrapezoidSafe(lastDelta, curDelta, nextDelta)
      pushDelta(nextDelta)
    } catch {
      case LookaheadFault =>
        recordFault(LookaheadFault)

        //could loop for a long time, but eventually we get below the epsilon
        require(nextDelta.f > Epsilon.e, "unable to handle LookaheadHalt")

        process(nextDelta scaleFr ResizeFactor)
    }
  }
}
