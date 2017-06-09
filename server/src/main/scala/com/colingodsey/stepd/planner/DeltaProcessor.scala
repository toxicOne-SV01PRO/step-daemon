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

  var frScale: Double = 0.25//1.0

  def processMoveDelta(delta: MoveDelta): Unit

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