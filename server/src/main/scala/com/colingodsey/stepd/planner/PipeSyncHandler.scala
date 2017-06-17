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

package com.colingodsey.stepd.planner

import com.colingodsey.stepd.Math.Vector4D
import akka.actor._
import com.colingodsey.stepd.serial.LineSerial

object PipeSyncHandler {
  case class Done(pos: Vector4D)
}

//this goofy class exists just so we can get information synced back to the pipeline
class PipeSyncHandler(deltaProcessor: ActorRef, lineSerial: ActorRef) extends Actor {
  context.system.eventStream.subscribe(self, classOf[LineSerial.Response])

  def receive: Receive = {
    case LineSerial.Response(str) if str.startsWith("X:") && str.contains(" Count ") =>
      //Recv: X:0.00 Y:0.00 Z:10.00 E:0.00 Count X:0 Y:0 Z:16000
      val parts = str.trim.split(' ')

      val x = parts(0).drop(2).toFloat
      val y = parts(1).drop(2).toFloat
      val z = parts(2).drop(2).toFloat
      val e = parts(3).drop(2).toFloat

      deltaProcessor ! PipeSyncHandler.Done(Vector4D(x, y, z, e))
  }
}