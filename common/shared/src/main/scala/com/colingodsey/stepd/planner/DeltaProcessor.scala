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

import com.colingodsey.stepd.GCode._
import com.colingodsey.stepd.Math.Vector4D

object DeltaProcessor {
  //absolute values
  case class Move(x: Double, y: Double, z: Double, e: Double, f: Double) extends Vector4D //feedrate per second

  //sends the current cartesian point down the pipeline
  case class SyncPos(pos: Vector4D)
}

//takes absolute positions and produces move deltas
trait DeltaProcessor {
  import DeltaProcessor._

  var pos = Vector4D.Zero
  private var _fr: Double = 0.0

  def fr = _fr

  def frScale: Double = 1.0

  def process(delta: MoveDelta): Unit

  def process(x: SyncPos): Unit

  def process(move: Move): Unit = {
    val d = MoveDelta(pos, move, move.f)

    pos = move

    //warm normal lazy val
    d.d.normal
    d.d.abs.normal

    process(d)
    process(SyncPos(pos))
  }

  def process(move: GMove): Unit = {
    val x = move.x.getOrElse(pos.x)
    val y = move.y.getOrElse(pos.y)
    val z = move.z.getOrElse(pos.z)
    val e = move.e.getOrElse(pos.e)
    val f = move.f.getOrElse(fr)

    _fr = f

    if(move.isFrOnly) None
    else process(Move(x, y, z, e, f / 60.0f * frScale))
  }

  def process(setPos: SetPos): Unit = {
    pos = Vector4D(
      setPos.x.getOrElse(pos.x),
      setPos.y.getOrElse(pos.y),
      setPos.z.getOrElse(pos.z),
      setPos.e.getOrElse(pos.e)
    )
    process(SyncPos(pos))
  }
}