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

import Math._
import akka.util.ByteString
import com.colingodsey.stepd.planner

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
  val halfDist = move.length * 0.5

  if(accelDist > halfDist) throw PreEaseLimit
  if(deccelDist > halfDist) throw PostEaseLimit

  require(accelTime >= 0, this.toString)
  require(deccelTime >= 0, this.toString)
  require(accelDist >= 0, this.toString)
  require(deccelDist >= 0, this.toString)

  def getPos(t: Double): Double = {
    val ret = if(t < accelTime)
      frAccel * t * t * 0.5 + frStart * t
    else if(t >= deccelStartTime) {
      val dt = t - deccelStartTime

      deccelPos + frDeccel * dt * dt * 0.5 + move.f * dt
    } else {
      val dt = t - accelTime

      accelDist + dt * move.f
    }

    clamp(0.0, ret, move.length)
  }

  def posIterator(tickRate: Double): Iterator[Position] = new Iterator[Position] {
    val ticks = (time * tickRate).toInt
    val div = time / ticks

    var tick = 0

    def hasNext: Boolean = tick < ticks

    def next(): Position = {
      val x = if(tick == 0) move.from
      else move.from + move.d.normal * getPos(tick * div)

      tick += 1

      x
    }
  }
}

object MoveDelta {
  val Empty = MoveDelta(Position.Zero, Position.Zero, 0)
}

final case class MoveDelta(from: Position, to: Position, f: Double) {
  val d = to - from
  val time = (d.length / f)
  val v = d / time
  val isValid = d.length > 0

  //warm normal lazy val
  d.normal

  def length = d.length

  def isEOrZOnly = d.x == 0 && d.y == 0

  def scaleFr(scale: Double) = copy(f = f * scale)
}

object Chunk {
  val chunkHeader = ByteString.fromString("!")
  val chunkFooter = ByteString.empty //ByteString.fromString("\r\n")
}

case class Chunk(rawBytes: ByteString) {
  import Chunk._

  require(rawBytes.length == StepProcessor.BytesPerChunk)

  val check = rawBytes.foldLeft(0)(_ ^ _) & 0xFF
  val chunk = chunkHeader ++ rawBytes ++ ByteString(check.toByte) ++ chunkFooter

  def length = chunk.length
}
