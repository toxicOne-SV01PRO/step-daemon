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

import planner.Math._

object Parser {
  sealed trait GCodeCommand {
    def raw: Raw
    def isGCommand: Boolean
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

  def calcChecksum(str: String): Int =
    str.iterator.map(_.toByte).foldLeft(0)(_ ^ _) & 0xFF
}

object CommandParser {
  import Parser._

  sealed trait MCommand extends GCodeCommand {
    def isGCommand = false
  }
  sealed trait GCommand extends GCodeCommand {
    def isGCommand = true
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

    def apply(pos: Position): SetPos = {
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

trait CommandParser {
  import CommandParser._
  import Parser._

  def process(cmd: GCodeCommand): Unit

  def process(raw: Raw): Unit = {
    val out: GCodeCommand = raw.cmd match {
      case "G0" | "G1" => GMove(raw)
      case "G92" => SetPos(raw)
      case "G28" | "G29" =>
        //get position after other G commands
        process(raw: GCodeCommand)
        M114
      case "M114" => M114
      case _ => raw
    }

    process(out)
  }
}

trait Parser {
  import Parser._

  val buffer = new Array[Char](1024)
  var idx = 0
  var lastN = 0

  def process(cmd: Raw): Unit

  def process(line: String): Unit = if(line.trim.nonEmpty) {
    val cmd = line.indexOf('*') match {
      case -1 => line
      case idx =>
        val cmd = line.substring(0, idx)
        val checksum = line.substring(idx + 1).toInt

        if(checksum != calcChecksum(cmd))
          sys.error("Failed checksum! Last N: " + lastN)

        cmd
    }

    val raw = Raw(cmd.trim)

    //set n if exists
    raw.getPart('N').map(_.toInt).foreach(lastN = _)

    process(raw)
  }

  def process(char: Char): Unit = char match {
    case '\r' | '\n' if idx != 0 =>
      val line = buffer.iterator.take(idx).mkString
      idx = 0

      //remove comments
      line.indexOf(';') match {
        case -1 => process(line)
        case 0 => //ignore
        case n => process(line.substring(0, n))
      }
    case '\r' | '\n' => //ignore if start of line
    case _ if idx == buffer.length =>
      sys.error("Buffer overflow!")
    case c =>
      buffer(idx) = c
      idx += 1
  }
}
