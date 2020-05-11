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
import com.colingodsey.stepd.GCode.Command
import com.colingodsey.stepd.planner._
import com.colingodsey.stepd.serial.{LineSerial, SerialDeviceActor}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.control.NoStackTrace

/**
 * The flow:
 *
 * proxy -> publish(Command)          -> pipeline -> device
 *                                    -> bed-level
 *
 * device -> publish(Response)        -> pages
 *                                    -> bed-level
 *                                    -> proxy
 *                                    -> delta
 *
 * device -> publish(ControlResponse) -> pages
 */
object PrintPipeline {
  sealed trait Signal

  sealed trait InputFlowSignal extends Signal
  case object PauseInput extends InputFlowSignal
  case object ResumeInput extends InputFlowSignal

  case class Completed(cmd: Command)

  sealed trait Response
  case class TextResponse(str: String) extends Response
  case class ControlResponse(data: ByteString) extends Response

  case object Restart extends NoStackTrace
}

class PrintPipeline(proxy: ActorRef, device: ActorRef) extends Actor {
  val chunkManager = context.actorOf(Props[PageManagerActor], name="pages")
  val steps = context.actorOf(Props(classOf[StepProcessorActor], chunkManager, ConfigMaker.plannerConfig), name="steps")
  val physics = context.actorOf(Props(classOf[PhysicsProcessorActor], steps, ConfigMaker.plannerConfig), name="physics")
  val delta = context.actorOf(Props(classOf[DeltaProcessorActor], physics, false), name="delta")

  override val supervisorStrategy =
    OneForOneStrategy(maxNrOfRetries = 10, withinTimeRange = 1.minute) {
      case _: Throwable => SupervisorStrategy.Escalate
    }

  context watch proxy
  context watch device

  context.system.eventStream.subscribe(self, classOf[Command])

  def receive = {
    // tail of pipeline
    case x: Command if sender == chunkManager => device.tell(x, sender)
    case x: LineSerial.Bytes if sender == chunkManager => device.tell(x, sender)

    // head of pipeline, from proxy
    case x: Command => delta.tell(x, sender)
  }
}

object Server extends App {
  val system = ActorSystem("stepd", ConfigMaker.config)

  sys.addShutdownHook {
    Await.result(system.terminate(), 10.seconds)
  }

  //prevent AWT from opening a window
  System.setProperty("java.awt.headless", "true")

  {
    val device = system.actorOf(Props(classOf[SerialDeviceActor], ConfigMaker.deviceConfig), name="device")
    val proxy = system.actorOf(Props(classOf[SocatProxy]), name="proxy")

    system.actorOf(Props(classOf[MeshLevelingActor], ConfigMaker.levelingConfig), name="bed-level")
    system.actorOf(Props(classOf[PrintPipeline], proxy, device), name="pipeline")
  }
}
