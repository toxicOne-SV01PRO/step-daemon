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
import jssc.{SerialPort, SerialPortList}

import scala.concurrent.blocking

object Serial {
  object Bytes {
    def apply(x: String): Bytes =
      Bytes(ByteString fromString x)
  }
  case class Bytes(data: ByteString)

  sealed trait FlowCommand
  case object PauseRead extends FlowCommand
  case object ResumeRead extends FlowCommand

  class Reader(port: SerialPort) extends Actor with Stash with ActorLogging {
    self ! Read

    //TODO: the pure blocking form really is the best way to do serial. but JSSC sucks, so we're screwed
    /*def read(): Unit = {
      val available = port.getInputBufferBytesCount
      val toRead = math.max(math.min(available, 1024), 1)

      val dat = blocking(port.readBytes(toRead))

      context.parent ! Serial.Bytes(ByteString.empty ++ dat)
    }*/

    def read(): Unit = {
      val firstByte = try blocking(port.readBytes(1, 1000)) catch {
        case _: jssc.SerialPortTimeoutException => null
      }

      val available = port.getInputBufferBytesCount
      val toRead = math.min(available, 1024)

      firstByte match {
        case null =>
        case firstByte =>
          val out = blocking(port.readBytes(toRead)) match {
            case null => ByteString.empty ++ firstByte
            case bytes => ByteString.empty ++ firstByte ++ bytes
          }

          context.parent ! Serial.Bytes(out)
      }
    }

    def paused: Receive = {
      case ResumeRead =>
        log debug "resumed"
        context become normal
        unstashAll()
      case _ =>
        stash()
    }

    def normal: Receive = {
      case Read =>
        read()

        self ! Read
      case PauseRead =>
        log debug "paused"
        context become paused
    }

    def receive = normal

    object Read
  }

  class Writer(port: SerialPort) extends Actor with ActorLogging {
    def receive = {
      case Serial.Bytes(dat) =>
        val wrote = blocking(port.writeBytes(dat.toArray))

        require(wrote, "failed write")
    }
  }
}

class Serial(cfg: DeviceConfig) extends Actor with ActorLogging {
  import Serial._

  val port = initPort
  val buffer = new Array[Byte](1024)
  val parent = context.parent

  val writer = context.actorOf(
    Props(classOf[Writer], port).withDispatcher("akka.io.pinned-dispatcher"),
    name = "writer")

  val reader = context.actorOf(
    Props(classOf[Reader], port).withDispatcher("akka.io.pinned-dispatcher"),
    name = "reader")

  def initPort = {
    log.info {
      val ports = SerialPortList.getPortNames().toSeq

      ports.toString
    }

    val port = new SerialPort(cfg.dev)

    port.openPort()
    port.setParams(cfg.baud,
      SerialPort.DATABITS_8,
      SerialPort.STOPBITS_1,
      SerialPort.PARITY_NONE)

    require(port.isOpened, "failed to open port")

    port
  }

  //the exciting life of a proxy actor
  def receive = {
    case x: Serial.Bytes if sender == reader =>
      parent ! x
    case x: Serial.Bytes =>
      writer ! x
    case x: FlowCommand =>
      reader ! x
  }

  override def postStop(): Unit = {
    super.postStop()

    if(port.isOpened) port.closePort()
  }
}