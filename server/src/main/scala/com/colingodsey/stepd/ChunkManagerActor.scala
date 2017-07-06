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

package com.colingodsey.stepd

import akka.actor._
import akka.util.ByteString
import com.colingodsey.stepd.GCode._
import com.colingodsey.stepd.planner._
import com.colingodsey.stepd.serial.{LineSerial, SerialGCode}

class ChunkManagerActor(gcodeSerial: ActorRef, maxChunks: Int) extends Actor with Stash with ActorLogging with Pipeline.Terminator {
  val maxGCodeQueue = 4

  var firstChunkIdx = -1
  var isPendingChunk = false
  var sent = 0
  var lastChunk: Chunk = null

  var pendingCommands = 0

  context.system.eventStream.subscribe(self, classOf[LineSerial.Response])

  context become waitStart

  def isPending = pendingCommands >= maxGCodeQueue || isPendingChunk

  def flushChunks(): Unit = if(sent > 0) {
    require(firstChunkIdx != -1)

    log debug "flush chunk"

    sendGCodeCommand(CMove(firstChunkIdx, sent))

    firstChunkIdx = -1
    sent = 0
  }

  def sendGCodeCommand(cmd: Command): Unit = {
    gcodeSerial ! SerialGCode.Command(cmd.raw.line)

    log.debug("send: {}", cmd.raw.line)

    pendingCommands += 1
  }

  def waitStart: Receive = {
    case LineSerial.Response(str) if str.startsWith("setup_done") =>
      context become receive

      log info "starting"

      unstashAll()
    case LineSerial.Response(str) =>
      log.info("recv: {}", str)
    case _ =>
      stash()
  }

  def receive: Receive = {
    case LineSerial.Response(str) if str.startsWith("!busy") =>
      require(lastChunk != null && isPendingChunk)

      log debug "busy resend"

      //TODO: should consider an async wait cycle here if we can do it fast enough
      scala.concurrent.blocking(Thread.sleep(1))

      gcodeSerial ! LineSerial.Bytes(lastChunk.chunk)
    case LineSerial.Response(str) if str.startsWith("!ok") =>
      val idx = str.drop(4).trim.toInt

      if(firstChunkIdx == -1)
        firstChunkIdx = idx

      require(isPendingChunk, "stray !ok")

      isPendingChunk = false
      lastChunk = null

      if(sent == maxChunks)
        flushChunks()
      else
        unstashAll()

      log debug "chunk ok"
    case LineSerial.Response(str) if str.startsWith("!fail") =>
      log.error("chunk fail {}", str)

      require(lastChunk != null && isPendingChunk)

      gcodeSerial ! LineSerial.Bytes(lastChunk.chunk)
    case LineSerial.Response(str) if str.startsWith("ok N") =>
      log.debug("ok: {}", str)
    case LineSerial.Response(str) =>
      log.info("recv: {}", str)

    case _: Chunk if isPending =>
      stash()
    case chunk: Chunk =>
      ack()

      log debug "send chunk"

      lastChunk = chunk
      gcodeSerial ! LineSerial.Bytes(lastChunk.chunk)

      isPendingChunk = true
      sent += 1

    case cmd: Command if isPending =>
      stash()
    case cmd: Command =>
      ack()

      flushChunks()

      sendGCodeCommand(cmd)

    case syncPos: StepProcessor.SyncPos =>
      ack()

    case SerialGCode.Completed(_) =>
      require(pendingCommands > 0, "more oks than sent commands!")

      log debug "ok"

      pendingCommands -= 1

      unstashAll()
  }
}
