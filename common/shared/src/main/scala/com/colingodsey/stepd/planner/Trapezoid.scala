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

final case class Trapezoid(frStart: Double, frAccel: Double, move: MoveDelta, frDeccel: Double, frEnd: Double) {
  val accelDf = move.f - frStart
  val accelTime = if(frAccel == 0) 0.0 else accelDf / frAccel
  val accelDist = frAccel * accelTime * accelTime * 0.5 + frStart * accelTime

  val deccelDf = frEnd - move.f
  val deccelTime = if(frDeccel == 0) 0.0 else deccelDf / frDeccel
  val deccelDist = frDeccel * deccelTime * deccelTime * 0.5 + move.f * deccelTime

  val coastDist = move.length - deccelDist - accelDist
  val coastTime = coastDist / move.f

  val deccelPos = move.length - deccelDist
  val deccelStartTime = accelTime + coastTime

  val time = accelTime + coastTime + deccelTime
  //val halfDist = move.length * 0.5

  //TODO: instead of comparing half dist, make sure the sum is less than move.length
  //if(accelDist > halfDist) throw PreEaseLimit
  //if(deccelDist > halfDist) throw PostEaseLimit

  if((accelDist + deccelDist) > move.length) {
    if(accelDist > deccelDist) throw PreEaseLimit
    else throw PostEaseLimit
  }

  require(accelTime >= 0, (accelTime, this).toString)
  require(deccelTime >= 0, (deccelTime, this).toString)
  require(accelDist >= 0, (accelDist, this).toString)
  require(deccelDist >= 0, (deccelDist, this).toString)

  def getPos(t: Double): Double = {
    val ret = if (t < accelTime) {
      frAccel * t * t * 0.5 + frStart * t
    } else if(t >= deccelStartTime) {
      val dt = t - deccelStartTime

      deccelPos + frDeccel * dt * dt * 0.5 + move.f * dt
    } else {
      val dt = t - accelTime

      accelDist + dt * move.f
    }

    clamp(0.0, ret, move.length)
  }

  def posIterator(tickRate: Double): Iterator[Vec4] = new Iterator[Vec4] {
    val ticks = (time * tickRate).toInt
    val div = time / ticks

    var tick = 0

    def hasNext: Boolean = tick < ticks

    def next(): Vec4 = {
      val x = if(tick == 0) move.from
      else move.from + move.d.normal * getPos(tick * div)

      tick += 1

      x
    }
  }
}
