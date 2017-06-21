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

import java.io.File

import com.colingodsey.stepd.Math.Vector4D
import com.colingodsey.stepd.planner.{DeviceConfig, MeshLevelingConfig, PlannerConfig}
import com.typesafe.config.{Config, ConfigFactory}

import scala.collection.JavaConversions._

object ConfigMaker {
  val config = ConfigFactory.parseFile(new File("./config.conf")) withFallback
      ConfigFactory.load()

  val stepd = config.getConfig("com.colingodsey.stepd")
  val planner = stepd.getConfig("planner")
  val device = stepd.getConfig("device")
  val bed = stepd.getConfig("bed")

  def plannerConfig = {
    val accel = planner.getDoubleList("acceleration")
    val jerk = planner.getDoubleList("jerk")
    val stepsPerMM = planner.getDoubleList("steps-per-mm")

    PlannerConfig(
      accel = Vector4D(accel(_)),
      jerk = Vector4D(jerk(_)),
      stepsPerMM = Vector4D(stepsPerMM(_)),
      ticksPerSecond = planner.getInt("ticks-per-second")
    )
  }

  def deviceConfig = {
    DeviceConfig(
      dev = device.getString("dev"),
      baud = device.getInt("baud")
    )
  }

  def levelingConfig = {
    MeshLevelingConfig(
      bedMaxX = bed.getInt("max-x"),
      bedMaxY = bed.getInt("max-y")
    )
  }
}
