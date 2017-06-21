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

import com.colingodsey.stepd.GCode.SetPos
import com.colingodsey.stepd.Math.Vector4D

object StepProcessor {
  final val BytesPerChunk = 256
  final val BytesPerBlock = 2
  final val StepsPerBlock = 8 //actually 7, but we math it at 3 bits

  final val BlocksPerChunk = BytesPerChunk / BytesPerBlock
  final val StepsPerChunk = BlocksPerChunk * StepsPerBlock
}

trait StepProcessor {
  import StepProcessor._

  def stepsPerMM: Vector4D
  def ticksPerSecond: Int
  def leveling: MeshLevelingReader

  var stepPosX = 0L
  var stepPosY = 0L
  var stepPosZ = 0L
  var stepPosE = 0L

  //var lastPos = Position.Zero

  var currentChunk = new Array[Byte](BytesPerChunk)
  var chunkIndex = 0

  //stats
  def recordSplit(axis: Int): Unit
  def processChunk(chunk: Array[Byte]): Unit

  def setPos(setPos: SetPos): Unit = {
    //TODO: if setting Z, should we reference the leveling offset?
    //val zOffs = leveling.getOffset(pos.x.toFloat, pos.y.toFloat)

    setPos.x.foreach(x => stepPosX = (x * stepsPerMM.x).round)
    setPos.y.foreach(y => stepPosY = (y * stepsPerMM.y).round)
    setPos.z.foreach(z => stepPosZ = (z * stepsPerMM.z).round)
    setPos.e.foreach(e => stepPosE = (e * stepsPerMM.e).round)
  }

  def flushChunk(): Unit = if(chunkIndex > 0) {
    while(chunkIndex < BytesPerChunk) {
      //fill rest with 'zero' moves
      currentChunk(chunkIndex) = ((7 << 4) | 7).toByte
      chunkIndex += 1
    }

    chunkIndex = 0

    processChunk(currentChunk.clone())
  }

  def addSegment(a: Byte, b: Byte): Unit = {
    currentChunk(chunkIndex) = a
    chunkIndex += 1
    currentChunk(chunkIndex) = b
    chunkIndex += 1

    if(chunkIndex == BytesPerChunk) flushChunk()
  }

  def processSegment(dx: Byte, dy: Byte, dz: Byte, de: Byte): Unit = {
    var a, b: Int = 0

    a |= (dx + 7) << 4
    a |= dy + 7
    b |= (dz + 7) << 4
    b |= de + 7

    addSegment(a.toByte, b.toByte)
  }

  def processMove(dx: Int, dy: Int, dz: Int, de: Int): Unit = {
    if(math.abs(dx) >= StepsPerBlock || math.abs(dy) >= StepsPerBlock ||
        math.abs(dz) >= StepsPerBlock || math.abs(de) >= StepsPerBlock) {
      //we must break the delta into 2 moves
      val dxl = dx / 2
      val dyl = dy / 2
      val dzl = dz / 2
      val del = de / 2

      val dxr = dx - dxl
      val dyr = dy - dyl
      val dzr = dz - dzl
      val der = de - del

      if(math.abs(dx) >= StepsPerBlock) recordSplit(0)
      if(math.abs(dy) >= StepsPerBlock) recordSplit(1)
      if(math.abs(dz) >= StepsPerBlock) recordSplit(2)
      if(math.abs(de) >= StepsPerBlock) recordSplit(3)

      processMove(dxl, dyl, dzl, del)
      processMove(dxr, dyr, dzr, der)
    } else {
      processSegment(dx.toByte, dy.toByte, dz.toByte, de.toByte)
    }
  }

  def process(trap: Trapezoid): Unit = {
    val iter = trap.posIterator(ticksPerSecond / StepsPerBlock)

    for(pos <- iter) {
      val zOffset = leveling.getOffset(pos.x, pos.y)

      val stepPosXDest = (pos.x             * stepsPerMM.x).round
      val stepPosYDest = (pos.y             * stepsPerMM.y).round
      val stepPosZDest = ((pos.z + zOffset) * stepsPerMM.z).round
      val stepPosEDest = (pos.e             * stepsPerMM.e).round

      val dx = stepPosXDest - stepPosX
      val dy = stepPosYDest - stepPosY
      val dz = stepPosZDest - stepPosZ
      val de = stepPosEDest - stepPosE

      processMove(dx.toInt, dy.toInt, dz.toInt, de.toInt)

      //lastPos = pos
      stepPosX = stepPosXDest
      stepPosY = stepPosYDest
      stepPosZ = stepPosZDest
      stepPosE = stepPosEDest
    }
  }
}
