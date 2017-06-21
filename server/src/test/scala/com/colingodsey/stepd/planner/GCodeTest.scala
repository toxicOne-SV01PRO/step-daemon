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

import akka.util.ByteString
import com.colingodsey.stepd.Math.Vector4D
import com.colingodsey.stepd._

object GCodeTest {
  class TestPipeline(resourceName: String) extends Parser with CommandParser
      with StepProcessor with DeltaProcessor with PhysicsProcessor {
    val stream = getClass.getResourceAsStream(resourceName) //"/g_test1.gcode")
    val gcode = scala.io.Source.fromInputStream(stream).getLines.mkString("\r\n")

    val acc = Vector4D(2000, 1500, 100, 10000)
    val jerk = Vector4D(15, 10, 0.4f, 5)
    val stepsPerMM = Vector4D(80, 80, 1600, 95.2)
    val ticksPerSecond: Int = 30000

    def leveling: MeshLevelingReader = MeshLevelingReader.Empty

    def recordSplit(axis: Int): Unit = {}

    def recordFault(fault: Math.MathFault): Unit = {}

    def process(x: DeltaProcessor.SyncPos): Unit = {}

    def processChunk(chunk: Array[Byte]): Unit = ???

    def process(cmd: GCode.GCodeCommand): Unit = ???
  }
}
