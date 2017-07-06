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
import com.colingodsey.stepd.planner.{MeshLeveling, MeshLevelingConfig, MeshLevelingReader}
import com.colingodsey.stepd.serial.SerialGCode
import json._

import scala.util.control.NonFatal

object MeshLevelingActor {
  val configPath = "bedlevel.json"
}

class MeshLevelingActor(cfg: MeshLevelingConfig) extends Actor with ActorLogging {
  import MeshLevelingActor._

  context.system.eventStream.subscribe(self, classOf[SerialGCode.Response])
  context.system.eventStream.subscribe(self, classOf[SerialGCode.Command])

  var pointsBuffer = Set[MeshLeveling.Point]()
  var currentLeveling: Option[MeshLeveling] = None
  var isReading = false

  def loadFromFile(): Unit = try {
    val str = Util.readFile(configPath).trim

    require(pointsBuffer.isEmpty)

    if(str.nonEmpty) {
      val points = str.parseJSON.toObject[Seq[MeshLeveling.Point]]

      pointsBuffer ++= points

      flushPoints()
    }
  } catch {
    case NonFatal(t) =>
      log.error(t, "Failed to load " + configPath)
  }

  def sendReader(): Unit = {
    val reader: MeshLevelingReader = currentLeveling match {
      case Some(x) => x.reader()
      case None => MeshLevelingReader.Empty
    }

    context.system.eventStream.publish(reader)
  }

  def saveToFile(): Unit = if(currentLeveling.isDefined) {
    val js: JValue = currentLeveling.get.points.js

    Util.writeFile(configPath, js.toPrettyString)
  } else log.warning("no meshleveling to save!")

  def flushPoints(): Unit = {
    log.info("processing bed level points")

    currentLeveling = Some(MeshLeveling(pointsBuffer.toSeq, cfg.bedMaxX, cfg.bedMaxY))

    pointsBuffer = pointsBuffer.empty
    isReading = false

    sendReader()
    saveToFile()
  }

  def receive = {
    case SerialGCode.Response(MeshLeveling.OutputLine(point)) if isReading =>
      pointsBuffer += point
    case SerialGCode.Response(line @ MeshLeveling.OutputLine(_)) =>
      log.warning("Not expecting bed point: " + line)
    case SerialGCode.Command(cmd) if cmd.startsWith("G29 ") =>
      require(!isReading, "started bed leveling before finishing the last")

      log.info("gathering bed level points")

      isReading = true
  }

  override def preStart(): Unit = {
    loadFromFile()

    super.preStart()
  }
}