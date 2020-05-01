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

package com.colingodsey.stepd.serial

import akka.actor._
import akka.util.ByteString

import com.colingodsey.stepd.planner.DeviceConfig

import scala.concurrent.duration._

object SerialGCode {
  case class Command(cmd: String)
  case class Completed(cmd: Command)

  type Response = LineSerial.Response
  val Response = LineSerial.Response

  type ControlResponse = LineSerial.ControlResponse
  val ControlResponse = LineSerial.ControlResponse

  val MaxQueue = 4
  //val MaxQueue = 1
  val MaxIncr = 99
  val NumReset = "M110"
}

class SerialGCode(cfg: DeviceConfig) extends Actor with Stash with ActorLogging {
  import SerialGCode._

  var nIncr = 1
  var pending = Map[Int, (ActorRef, Command)]()

  val lineSerial: ActorRef = context.actorOf(
    Props(classOf[LineSerial], cfg),
    name = "line-serial")

  override def preStart(): Unit = {
    super.preStart()

    lineSerial ! Serial.Bytes(NumReset + "\r\n")
  }

  def sendCommand(str: String) = {
    val str0 = s"N$nIncr $str"
    val bytes = ByteString.fromString(str0)
    val check = bytes.foldLeft(0)(_ ^ _) & 0xFF
    val finalBytes = bytes ++ ByteString.fromString(s"*$check\r\n")

    log.info("send: {}*{}", str0, check)

    lineSerial ! Serial.Bytes(finalBytes)
  }

  def receive = {
    case _: Command if pending.size >= MaxQueue =>
      stash()
    case cmd @ Command(str0) =>
      val str = str0.trim

      if(nIncr > MaxIncr) {
        nIncr = 1
        sendCommand(NumReset)
        nIncr += 1
      }

      sendCommand(str)
      pending += nIncr -> (sender, cmd)

      nIncr += 1

      context.system.eventStream.publish(cmd)

    case x: LineSerial.Bytes =>
      lineSerial ! x

    case x: Serial.FlowCommand =>
      lineSerial ! x

    case a @ Response(str) if str.startsWith("ok N") || str.startsWith("ok T") =>
      unstashAll()

      log.debug("ok: {}", str)

      if (str.startsWith("ok N")) {
        val n = str.drop(4).takeWhile(_ != ' ').toInt

        pending get n match {
          case None if n > 1 =>
            log.warning("Got an ok, but no sender to respond to")
          case None =>
          case Some((ref, cmd)) =>
            ref ! Completed(cmd)
        }

        pending -= n
      }

      //goofy M105 response
      if(str.startsWith("ok T"))
        context.system.eventStream.publish(a)
    case Response(str) if str.startsWith("ok N") =>
      log.warning("Got an ok for no reason: {}", str)
    case Response(str) if str.startsWith("ok") =>
      log.warning("Random ok with no N value: {}", str)
    //any other line we broadcast out to our subscribers
    case a: Response => context.system.eventStream.publish(a)
    case a: ControlResponse => context.system.eventStream.publish(a)
  }
}