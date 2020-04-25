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

import java.util.Scanner

import akka.actor._
import Math._
import com.colingodsey.stepd.GCode.Command
import com.colingodsey.stepd.Server.system
import com.colingodsey.stepd.planner._
import com.colingodsey.stepd.serial.{LineSerial, SerialGCode}

import scala.concurrent.Await
import scala.concurrent.duration._

class ServerActor extends Actor {
  val gcodeSerial = context.actorOf(Props(classOf[SerialGCode], ConfigMaker.deviceConfig), name="gcode-serial")
  val chunkManager = context.actorOf(Props(classOf[ChunkManagerActor], gcodeSerial, 5), name="chunk-manager")
  val steps = context.actorOf(Props(classOf[StepProcessorActor], chunkManager, ConfigMaker.plannerConfig), name="steps")
  val physics = context.actorOf(Props(classOf[PhysicsProcessorActor], steps, ConfigMaker.plannerConfig), name="physics")
  val delta = context.actorOf(Props(classOf[DeltaProcessorActor], physics, false), name="delta")
  val proxy = context.actorOf(Props(classOf[SocatProxy], delta), name="proxy")
  val bedlevel = context.actorOf(Props(classOf[MeshLevelingActor], ConfigMaker.levelingConfig), name="bed-leveling")

  def receive = PartialFunction.empty

  override def postStop(): Unit = {
    super.postStop()

    system.terminate()
  }
}

object Server extends App {
  //prevent AWT from opening a window
  System.setProperty("java.awt.headless", "true")

  val system = ActorSystem("stepd", ConfigMaker.config)

  val serverActor = system.actorOf(Props[ServerActor], name="server")

  sys.addShutdownHook {
    system.terminate()

    Await.result(system.whenTerminated, 10.seconds)
  }
}
