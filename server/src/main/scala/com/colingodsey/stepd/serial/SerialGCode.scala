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

object SerialGCode {
  case class Command(cmd: String)
  case class Completed(cmd: Command)

  type Response = LineSerial.Response
  val Response = LineSerial.Response

  val MaxQueue = 1
  val MaxIncr = 998
  val NumReset = "M110 N0"
}

class SerialGCode(cfg: DeviceConfig) extends Actor with Stash with ActorLogging {
  import SerialGCode._

  var nIncr = 1
  var waitOk = 0
  var lastMsgAndSender: Option[(ActorRef, Command)] = None

  val lineSerial: ActorRef = context.actorOf(
    Props(classOf[LineSerial], cfg),
    name = "line-serial")

  override def preStart(): Unit = {
    super.preStart()

    //TODO: does this initial one ever respond with an OK?
    lineSerial ! Serial.Bytes(NumReset + "\r\n")
  }

  //TODO: does this respond with an ok?
  def resetNumbering(): Unit =
    self ! Command(NumReset)

  //TODO: handle fail retries using lastMsgAndSender. Or just hope it doesnt fail for now ;)

  def receive = {
    case _: Command if waitOk >= MaxQueue =>
      stash()
    case cmd @ Command(str0) =>
      val str = str0.trim

      if(nIncr > MaxIncr)
        resetNumbering()

      val withN = s"N$nIncr $str"
      val withNBytes = ByteString.fromString(withN)
      val check = withNBytes.foldLeft(0)(_ ^ _) & 0xFF
      val finalBytes = withNBytes ++ ByteString.fromString(s"*$check\r\n")

      log.info("send: {}*{}", withN, check)

      lineSerial ! Serial.Bytes(finalBytes)

      lastMsgAndSender = Some(sender, cmd)

      if(str == NumReset) nIncr = 1
      else nIncr += 1

      waitOk += 1

      context.system.eventStream.publish(cmd)

    case x: LineSerial.Bytes =>
      lineSerial ! x

    case x: Serial.FlowCommand =>
      lineSerial ! x

    case a @ Response(str) if str.startsWith("ok N") || str.startsWith("ok T") =>
      waitOk -= 1
      unstashAll()

      log.info("ok: {}", str)

      //TODO: lets actually check N values here...

      lastMsgAndSender match {
        case None =>
          log.warning("Got an ok, but no sender to respond to")
        case Some((ref, cmd)) =>
          ref ! Completed(cmd)
      }

      lastMsgAndSender = None

      if(waitOk < 0) {
        log.warning("Received more OKs than commands sent")
        waitOk = 0
      }

      //goofy M105 response
      if(str.startsWith("ok T"))
        context.system.eventStream.publish(a)
    case Response(str) if str.startsWith("ok N") =>
      log.warning("Got an ok for no reason: {}", str)
    case Response(str) if str.startsWith("ok") =>
      log.warning("Random ok with no N value: {}", str)
    case a: Response =>
      //any other text line we broadcast out to our subscribers
      context.system.eventStream.publish(a)
  }
}