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

object LineSerial {
  case class Response(str: String)

  type Bytes = Serial.Bytes
  val Bytes = Serial.Bytes
}

class LineSerial(devName: String, baud: Int) extends Actor with ActorLogging with Stash {
  import LineSerial._

  log.info("starting...")

  val serial = context.actorOf(
    Props(classOf[Serial], devName, baud),
    name = "serial")

  val dataBuffer = new Array[Byte](1024)
  var bufferIdx = 0

  def processStr(str: String): Unit = {
    log.debug("recv: {}", str)

    context.parent ! Response(str)
  }

  def processByte(b: Byte): Unit = b.toChar match {
    case '\r' | '\n' if bufferIdx == 0 => //ignore
    case '\r' | '\n' =>
      val str = new String(dataBuffer, 0, bufferIdx, "UTF8")

      processStr(str)

      bufferIdx = 0
    case _ =>
      dataBuffer(bufferIdx) = b
      bufferIdx += 1
  }

  def receive = {
    case Serial.Bytes(data) if sender == serial =>
      data foreach processByte
    case x: Serial.Bytes =>
      serial ! x
  }
}