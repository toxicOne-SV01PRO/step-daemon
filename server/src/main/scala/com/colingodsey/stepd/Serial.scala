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
import jssc.{SerialPort, SerialPortList}
//import com.fazecast.jSerialComm._
//import gnu.io.NRSerialPort

import scala.concurrent._

object Serial {
  object Bytes {
    def apply(x: String): Bytes =
      Bytes(ByteString fromString x)
  }
  case class Bytes(data: ByteString)
}

class SerialReader(port: SerialPort) extends Actor {
  self ! Read

  def read(): Unit = {
    val firstByte = try blocking(port.readBytes(1, 1000)) catch {
      case _: jssc.SerialPortTimeoutException => null
    }

    firstByte match {
      case null =>
      case firstByte =>
        val out = blocking(port.readBytes()) match {
          case null => ByteString.empty ++ firstByte
          case bytes => ByteString.empty ++ firstByte ++ bytes
        }

        context.parent ! Serial.Bytes(out)
    }
  }

  def receive = {
    case Read =>
      read()

      self ! Read
  }

  object Read
}

class SerialWriter(port: SerialPort) extends Actor with ActorLogging {
  def receive = {
    case Serial.Bytes(dat) =>
      val wrote = blocking(port.writeBytes(dat.toArray))

      require(wrote, "failed write")
  }
}

class Serial extends Actor with ActorLogging {
  val port = initPort
  val buffer = new Array[Byte](1024)
  val parent = context.parent

  val writer = context.actorOf(
    Props(classOf[SerialWriter], port).withDispatcher("akka.io.pinned-dispatcher"),
    name = "writer")

  val reader = context.actorOf(
    Props(classOf[SerialReader], port).withDispatcher("akka.io.pinned-dispatcher"),
    name = "reader")

  def initPort = {
    log.info {
      val ports = SerialPortList.getPortNames().toSeq

      ports.toString
    }

    //val port = new SerialPort("/dev/tty.usbmodem1A1531")
    val port = new SerialPort("/dev/tty.usbserial-A9C7RPDD")

    port.openPort()
    port.setParams(500000,//250000,
      SerialPort.DATABITS_8,
      SerialPort.STOPBITS_1,
      SerialPort.PARITY_NONE)

    require(port.isOpened, "failed to open port")

    port
  }

  def receive = {
    case x: Serial.Bytes if sender == reader =>
      parent ! x
    case x: Serial.Bytes =>
      writer ! x
  }

  override def postStop(): Unit = {
    super.postStop()

    port.closePort()
  }
}

object LineSerial {
  case object Subscribe
  case class Response(str: String)

  type Bytes = Serial.Bytes
  val Bytes = Serial.Bytes
}

class LineSerial extends Actor with ActorLogging with Stash {
  import LineSerial._

  log.info("starting...")

  val serial = context.actorOf(
    Props(classOf[Serial]),
    name = "serial")

  val dataBuffer = new Array[Byte](1024)

  var subs = Set.empty[ActorRef]
  var bufferIdx = 0

  def processStr(str: String): Unit = {
    log.debug("recv: {}", str)

    val resp = Response(str)

    subs.foreach(_ ! resp)
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
    case Subscribe =>
      subs += sender
      context watch sender
    case Terminated(ref) if subs contains ref =>
      subs -= ref
  }
}

object SerialGCode {
  case class Command(cmd: String)
  case class Completed(cmd: Command)
}

class SerialGCode(printSerial: ActorRef) extends Actor with Stash with ActorLogging {
  import LineSerial._
  import SerialGCode._

  val MaxQueue = 1
  val maxIncr = 998
  val numReset = "M110 N0"

  var nIncr = 1
  var waitOk = 0
  var subs = Set.empty[ActorRef]
  var lastMsgAndSender: Option[(ActorRef, Command)] = None

  override def preStart(): Unit = {
    super.preStart()

    context watch printSerial

    printSerial ! Subscribe

    //resetNumbering()
    printSerial ! Serial.Bytes(numReset + "\r\n")
  }

  def resetNumbering(): Unit =
    self ! Command(numReset)

  //TODO: handle fail retries using lastMsgAndSender. Or just hope it doesnt fail for now ;)

  def receive = {
    case _: Command if waitOk >= MaxQueue =>
      stash()
    case cmd @ Command(str0) =>
      val str = str0.trim

      if(nIncr > maxIncr)
        resetNumbering()

      val withN = s"N$nIncr $str"
      val withNBytes = ByteString.fromString(withN)
      val check = withNBytes.foldLeft(0)(_ ^ _) & 0xFF
      val finalBytes = withNBytes ++ ByteString.fromString(s"*$check\r\n")

      log.info("send: {}*{}", withN, check)

      printSerial ! Serial.Bytes(finalBytes)

      lastMsgAndSender = Some(sender, cmd)

      if(str == numReset) nIncr = 1
      else nIncr += 1

      waitOk += 1
    case Response(str) if str.startsWith("ok") =>
      waitOk -= 1
      unstashAll()

      log.info("ok: {}", str)

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
    case Response(str) if str.startsWith("ok N") =>
      log.warning("Got an ok for no reason: " + str)
    case a: Response =>
      subs.foreach(_ ! a)
    case Subscribe =>
      subs += sender
      context watch sender
    case Terminated(ref) if subs contains ref =>
      subs -= ref
  }
}