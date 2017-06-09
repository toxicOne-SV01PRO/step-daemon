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
import com.colingodsey.stepd.Parser._
import com.colingodsey.stepd.planner._

class ChunkManagerActor(lineSerial: ActorRef, gcodeSerial: ActorRef, maxChunks: Int) extends Actor with Stash with ActorLogging {
  val maxGCodeQueue = 4

  var firstChunkIdx = -1
  var isPendingChunk = false
  var sent = 0
  var lastChunk: Chunk = null

  var pendingCommands = 0

  lineSerial ! LineSerial.Subscribe

  context become waitStart

  def isPending = pendingCommands >= maxGCodeQueue || isPendingChunk

  def flushChunks(): Unit = if(sent > 0) {
    require(firstChunkIdx != -1)

    log debug "flush chunk"

    sendGCodeCommand(CMove(firstChunkIdx, sent))

    firstChunkIdx = -1
    sent = 0
  }

  def sendGCodeCommand(cmd: GCodeCommand): Unit = {
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

      log debug "busy"

      //TODO: should consider an async wait cycle here so we're not just blasting away with data
      scala.concurrent.blocking(Thread.sleep(1))

      lineSerial ! LineSerial.Bytes(lastChunk.chunk)
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

      lineSerial ! LineSerial.Bytes(lastChunk.chunk)
    case LineSerial.Response(str) if str.startsWith("ok N") =>
      log.debug("ok: {}", str)
    case LineSerial.Response(str) =>
      log.debug("recv: {}", str)

    case _: ByteString if isPending =>
      stash()
    case rawChunk: ByteString =>
      sender ! Pipeline.Ack

      log debug "send chunk"

      lastChunk = Chunk(rawChunk)
      lineSerial ! LineSerial.Bytes(lastChunk.chunk)

      isPendingChunk = true
      sent += 1

    case cmd: GCodeCommand if isPending =>
      stash()
    case cmd: GCodeCommand =>
      sender ! Pipeline.Ack

      flushChunks()

      sendGCodeCommand(cmd)

    case SerialGCode.Completed(_) =>
      require(pendingCommands > 0, "more oks than sent commands!")

      log debug "ok"

      pendingCommands -= 1

      unstashAll()
  }
}
