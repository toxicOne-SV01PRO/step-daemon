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

import com.colingodsey.stepd.CommandParser._
import com.colingodsey.stepd.planner.Math._
import com.colingodsey.stepd.Parser.GCodeCommand

object DeltaProcessor {
  //absolute values
  case class Move(x: Double, y: Double, z: Double, e: Double, f: Double) extends Position //feedrate per second
}

//takes absolute positions and produces move deltas
trait DeltaProcessor {
  import DeltaProcessor._

  var pos = Position.Zero
  var fr: Double = 0.0f //per minute!!

  var frScale: Double = 1.0

  def processMoveDelta(delta: MoveDelta): Unit

  //TODO: adjust fr to account for internal fr, which uses E distance also?
  def processMove(move: Move): Unit = {
    val d = MoveDelta(pos, move, move.f)

    pos = move

    processMoveDelta(d)
  }

  def processGMove(move: GMove): Unit = {
    val x = move.x.getOrElse(pos.x)
    val y = move.y.getOrElse(pos.y)
    val z = move.z.getOrElse(pos.z)
    val e = move.e.getOrElse(pos.e)
    val f = move.f.getOrElse(fr)

    fr = f

    if(move.isFrOnly) None
    else processMove(Move(x, y, z, e, f / 60.0f * frScale))
  }

  def processSetPos(setPos: SetPos): Unit = {
    pos = Position(
      setPos.x.getOrElse(pos.x),
      setPos.y.getOrElse(pos.y),
      setPos.z.getOrElse(pos.z),
      setPos.e.getOrElse(pos.e)
    )
  }

  def getSetPos = SetPos(pos)

}