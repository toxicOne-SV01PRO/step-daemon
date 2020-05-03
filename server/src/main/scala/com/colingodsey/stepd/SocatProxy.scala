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
import com.colingodsey.stepd.GCode.Command
import com.colingodsey.stepd.planner.DeviceConfig
import com.colingodsey.stepd.serial.{LineSerial, Serial}

import scala.util.Try
import scala.concurrent.blocking

object SocatProxy {
  val socatLine = "socat -d -d pty,raw,echo=0 pty,raw,echo=0"

  val deviceBase = "/tmp/pty-stepd"
  val clientDevice = deviceBase + "-client"
  val serverDevice = deviceBase

  case class LineIn(str: String)
  case class PTY(dev: String)
}

class SocatProxy(val next: ActorRef) extends Actor with ActorLogging with Pipeline with LineParser with GCodeParser {
  import SocatProxy._
  import scala.sys.process._

  val logger = ProcessLogger(line => self ! LineIn(line))
  val ps = Process(socatLine).run(logger)

  var nLinked = 0
  var serialRef: Option[ActorRef] = None

  def processCommand(cmd: Command): Unit = {
    if (cmd.raw.cmd != "M110")
      sendDown(cmd)
  }

  def linkingDone(): Unit = {
    log.info("linking done")

    val devConf = DeviceConfig(serverDevice, 250000)

    val ref = context.actorOf(Props(classOf[Serial], devConf),
      name = "proxy-serial")

    serialRef = Some(ref)

    ref ! LineSerial.Bytes("start\n")

    context become normal
  }

  def sendOk(n: Option[Int]): Unit = n match {
    case None =>
      serialRef.get ! LineSerial.Bytes("ok\n")
    case Some(n) =>
      serialRef.get ! LineSerial.Bytes(s"ok N$n\n")
  }

  def normal: Receive = pipeline orElse {
    case Serial.Bytes(dat) =>
      //TODO: "sanitize '!' character"
      dat foreach process

    case LineSerial.Response(str) if str.startsWith("ok N") =>
    //case LineSerial.Response(str) if str.startsWith("ok ") =>
    case LineSerial.Response("start") => // block start from being sent
    case LineSerial.Response(str) if str.startsWith("!") =>
    case LineSerial.Response(str) =>
      serialRef.get ! LineSerial.Bytes(str)
      serialRef.get ! LineSerial.Bytes("\n")
  }

  def linking: Receive = pipeline orElse {
    case PTY(_) if nLinked == 2 =>
      sys.error("this is... unexpected")
    case PTY(dev) =>
      val target = if(nLinked == 0) clientDevice else serverDevice

      nLinked += 1

      log.info("Linking {} to {}", dev, target)

      blocking(s"ln -s $dev $target".!)

      if(nLinked == 2) linkingDone()

    case LineIn(str) if str.contains("N PTY") =>
      val dev = str.split(' ').last.trim

      log.debug("Pty {}", dev)

      self ! PTY(dev)
    case LineIn(str) =>
      log.info(str)
  }

  def receive = linking

  def logErr(e: Throwable): Unit = {
    log.error(e.toString)
  }

  override def preStart(): Unit = {
    super.preStart()

    context.system.eventStream.subscribe(self, classOf[LineSerial.Response])

    //incase we didnt shut down cleanly last time
    Try(s"rm $clientDevice".!)
    Try(s"rm $serverDevice".!)
  }

  override def onWake(): Unit = {
    serialRef.foreach(ref => ref ! Serial.ResumeRead)
    super.onWake()
  }

  override def onWait(): Unit = {
    serialRef.foreach(ref => ref ! Serial.PauseRead)
    super.onWait()
  }

  override def postStop(): Unit = blocking {
    super.postStop()

    Try(s"rm $clientDevice".!).failed foreach logErr
    Try(s"rm $serverDevice".!).failed foreach logErr

    ps.destroy()
    ps.exitValue()
  }
}
