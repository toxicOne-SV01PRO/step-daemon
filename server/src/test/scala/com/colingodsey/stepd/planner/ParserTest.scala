package com.colingodsey.stepd.planner

import utest._
import Math._
import com.colingodsey.stepd.Parser._
import com.colingodsey.stepd.CommandParser._
import com.colingodsey.stepd.CommandParser
import com.colingodsey.stepd.{CommandParser, Parser}

import scala.io.Source

object ParserTest extends TestSuite {
  val tests = this {
    'Test {
      val stream = getClass.getResourceAsStream("/g_test1.gcode")
      val testGCode = scala.io.Source.fromInputStream(stream).getLines.mkString("\r\n")

      var outCmds = Array.newBuilder[GCodeCommand]

      val expect = Set[GCodeCommand](
        GMove(Some(77.835f), Some(105.927f), None, Some(32.12709f), None)(null),
        GMove(Some(81.637f), Some(102.126f), None, Some(32.20156f), None)(null),
        GMove(Some(81.504f), Some(102.036f), None, Some(32.20378f), None)(null)
      )

      object testParser extends Parser with CommandParser {
        def process(cmd: Parser.GCodeCommand): Unit = {
          //println(cmd)
          outCmds += cmd
        }
      }

      testGCode foreach testParser.process

      val outSet = outCmds.result().toSet

      require((expect -- outSet).isEmpty)
    }
  }
}
