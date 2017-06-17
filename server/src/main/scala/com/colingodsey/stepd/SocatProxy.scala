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
import com.colingodsey.stepd.GCode.GCodeCommand
import com.colingodsey.stepd.planner.DeviceConfig
import com.colingodsey.stepd.serial.Serial

import scala.util.Try

object SocatProxy {
  val socatLine = "socat -d -d pty,raw,echo=0 pty,raw,echo=0"

  val deviceBase = "/tmp/pty-stepd"
  val clientDevice = deviceBase + "-client"
  val serverDevice = deviceBase
}

class SocatProxy(val next: ActorRef) extends Actor with ActorLogging with Pipeline with Parser with CommandParser {
  import SocatProxy._
  import scala.sys.process._

  val logger = ProcessLogger(line => self ! LineIn(line))
  val ps = Process(socatLine).run(logger)

  var nLinked = 0
  var serialRef: Option[ActorRef] = None

  case class LineIn(str: String)
  case class PTY(dev: String)

  def process(cmd: GCodeCommand): Unit = {
    sendDown(cmd)
  }

  def linkingDone(): Unit = {
    log.info("linking done")

    val devConf = DeviceConfig(serverDevice, 250000)

    val ref = context.actorOf(Props(classOf[Serial], devConf),
      name = "proxy-serial")

    serialRef = Some(ref)

    context become normal
  }

  def normal: Receive = {
    case Serial.Bytes(dat) =>
      dat foreach process
  }

  def linking: Receive = {
    case PTY(dev) if nLinked == 2 =>
      sys.error("this is... unexpected")
    case PTY(dev) =>
      val target = if(nLinked == 0) clientDevice else serverDevice

      nLinked += 1

      log.info("Linking {} to {}", dev, target)

      s"ln -s $dev $target".!

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

  override def onWake(): Unit = {
    serialRef.foreach(ref => ref ! Serial.ResumeRead)
    super.onWake()
  }

  override def onWait(): Unit = {
    serialRef.foreach(ref => ref ! Serial.PauseRead)
    super.onWait()
  }

  override def postStop(): Unit = {
    super.postStop()

    Try(s"rm $clientDevice".!).failed foreach logErr
    Try(s"rm $serverDevice".!).failed foreach logErr

    ps.destroy()
    ps.exitValue()
  }
}
