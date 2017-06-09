package com.colingodsey.stepd.planner

import akka.util.ByteString
import com.colingodsey.stepd.CommandParser.SetPos
import com.colingodsey.stepd.planner.Math.Position

object StepProcessor {
  trait StepSettings {
    def perUnit: Position

    def ticksPerSecond: Double
  }

  final val BytesPerChunk = 256
  final val BytesPerBlock = 2
  final val StepsPerBlock = 8 //actually 7, but we math it at 3 bits

  final val BlocksPerChunk = BytesPerChunk / BytesPerBlock
  final val StepsPerChunk = BlocksPerChunk * StepsPerBlock
}

trait StepProcessor {
  import StepProcessor._

  def steps: StepSettings
  def leveling: MeshLeveling.Reader

  var stepPosX = 0L
  var stepPosY = 0L
  var stepPosZ = 0L
  var stepPosE = 0L

  //var lastPos = Position.Zero

  var currentChunk = new Array[Byte](BytesPerChunk)
  var chunkIndex = 0

  //stats

  def recordSplit(axis: Int): Unit

  def processChunk(chunk: ByteString): Unit

  def setPos(setPos: SetPos): Unit = {
    //val zOffs = leveling.getOffset(pos.x.toFloat, pos.y.toFloat)

    setPos.x.foreach(x => stepPosX = (x * steps.perUnit.x).round)
    setPos.y.foreach(y => stepPosY = (y * steps.perUnit.y).round)
    setPos.z.foreach(z => stepPosZ = (z * steps.perUnit.z).round)
    setPos.e.foreach(e => stepPosE = (e * steps.perUnit.e).round)
  }

  def flushChunk(): Unit = if(chunkIndex > 0) {
    //val chunk = currentChunk.take(chunkIndex)

    while(chunkIndex < BytesPerChunk) {
      //fill rest with 'zero' moves
      currentChunk(chunkIndex) = ((7 << 4) | 7).toByte
      chunkIndex += 1
    }

    chunkIndex = 0

    processChunk(ByteString fromArray currentChunk)
  }

  def addBlock(a: Byte, b: Byte): Unit = {
    currentChunk(chunkIndex) = a
    chunkIndex += 1
    currentChunk(chunkIndex) = b
    chunkIndex += 1

    if(chunkIndex == BytesPerChunk) flushChunk()
  }

  def processBlock(dx: Byte, dy: Byte, dz: Byte, de: Byte): Unit = {
    var a, b: Int = 0

    a |= (dx + 7) << 4
    a |= dy + 7
    b |= (dz + 7) << 4
    b |= de + 7

    /*
    //TODO: THIS IS FOR TESTING ONLY! SWITCHES ZE/XY AXIS
    b |= (dx + 7) << 4
    b |= dy + 7
    a |= (dz + 7) << 4
    a |= de + 7*/

    addBlock(a.toByte, b.toByte)
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
      processBlock(dx.toByte, dy.toByte, dz.toByte, de.toByte)
    }
  }

  def processTrap(trap: Trapezoid): Unit = {
    val iter = trap.posIterator(steps.ticksPerSecond / StepsPerBlock)

    for(pos <- iter) {
      val zOffs = leveling.getOffset(pos.x.toFloat, pos.y.toFloat)

      val stepPosXDest = (pos.x * steps.perUnit.x).round
      val stepPosYDest = (pos.y * steps.perUnit.y).round
      val stepPosZDest = ((pos.z + zOffs) * steps.perUnit.z).round
      val stepPosEDest = (pos.e * steps.perUnit.e).round

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
