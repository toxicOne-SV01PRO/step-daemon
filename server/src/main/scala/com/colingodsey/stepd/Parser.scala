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

import com.colingodsey.stepd.GCode._

trait CommandParser {
  def process(cmd: GCodeCommand): Unit

  def process(raw: Raw): Unit = {
    val out: GCodeCommand = raw.cmd match {
      case "G0" | "G1" => GMove(raw)
      case "G92" => SetPos(raw)
      case "G28" =>
        //get position after homing
        process(raw: GCodeCommand)
        M114
      case "G29" =>
        //send the verbose version of the command
        process(Raw("G29 V3 T"))
        M114
      case "M114" => M114
      case _ => raw
    }

    process(out)
  }
}

trait Parser {
  private val buffer = new Array[Char](1024)
  private var idx = 0
  private var lastN = 0

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

  def process(byte: Byte): Unit =
    process(byte.toChar)

  def calcChecksum(str: String): Int =
    str.iterator.map(_.toByte).foldLeft(0)(_ ^ _) & 0xFF
}
