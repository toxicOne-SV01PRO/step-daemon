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

import com.colingodsey.stepd.Math.Vector4D

object GCode {
  sealed trait GCodeCommand {
    def raw: Raw
    def isGCommand: Boolean
  }
  sealed trait MCommand extends GCodeCommand {
    def isGCommand = false
  }
  sealed trait GCommand extends GCodeCommand {
    def isGCommand = true
  }

  case class Raw(line: String) extends GCodeCommand {
    private val split = line.split(' ').toStream.filter(_.nonEmpty)

    val cmd = split.head
    val parts = split.tail

    def raw = this

    def isGCommand = cmd(0) == 'G'

    def getPart(ident: Char): Option[String] =
      parts.filter(_.head == ident).headOption.map(_.tail)
  }

  case class CMove(chunkIdx: Int, nChunks: Int) extends GCodeCommand {
    val raw = Raw(s"C0 I$chunkIdx R$nChunks")

    def isGCommand: Boolean = false
  }

  object GMove {
    def apply(raw: Raw): GMove =
      GMove(
        raw.getPart('X').map(_.toDouble),
        raw.getPart('Y').map(_.toDouble),
        raw.getPart('Z').map(_.toDouble),
        raw.getPart('E').map(_.toDouble),
        raw.getPart('F').map(_.toDouble)
      )(raw)
  }

  case class GMove(x: Option[Double], y: Option[Double], z: Option[Double], e: Option[Double], f: Option[Double])(val raw: Raw) extends GCommand {
    def isFrOnly = x == None && y == None && z == None && e == None && f.isDefined
  }

  object SetPos {
    def apply(raw: Raw): SetPos =
      SetPos(
        raw.getPart('X').map(_.toDouble),
        raw.getPart('Y').map(_.toDouble),
        raw.getPart('Z').map(_.toDouble),
        raw.getPart('E').map(_.toDouble)
      )(raw)

    def apply(pos: Vector4D): SetPos = {
      val line = s"G92 X${pos.x.toFloat} Y${pos.y.toFloat} Z${pos.z.toFloat} E${pos.e.toFloat}"

      SetPos(
        Some(pos.x),
        Some(pos.y),
        Some(pos.z),
        Some(pos.e)
      )(Raw(line))
    }
  }

  case class SetPos(x: Option[Double], y: Option[Double], z: Option[Double], e: Option[Double])(val raw: Raw) extends GCommand

  case object M114 extends MCommand {
    val raw = Raw("M114")
  }
}
