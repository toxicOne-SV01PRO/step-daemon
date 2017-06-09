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

import org.apache.commons.math3.special.Erf

import scala.util.control.NoStackTrace

object Math {
  type Time = Double
  //type FP = Float
  //type Time = Float

  type Accel = Position
  type Jerk = Position

  final val sqrtPi = math.sqrt(math.Pi)
  final val halfSqrtPi = sqrtPi / 2.0
  final val twoOverPi = 2.0 / math.Pi
  final val quarterPi = math.Pi / 4.0

  //sigmoid with a slope of 1 at x = 0
  def sigmoidRaw(x: Double): Double =
    Erf.erf(x * halfSqrtPi)

  def sigmoid(x: Double, slope: Double): Double =
    sigmoidRaw(x * slope)

  def sigmoidIntRaw(x: Double): Double = {
    x * Erf.erf(x * halfSqrtPi) +
        twoOverPi * math.pow(math.E, -(quarterPi * x * x))
  }

  @inline final def clamp(min: Double, x: Double, max: Double): Double =
    math.min(math.max(x, min), max)

  object Position {
    @inline def apply(x: Double, y: Double, z: Double, e: Double): Position =
      Raw(x, y, z, e)

    private final case class Raw(x: Double, y: Double, z: Double, e: Double) extends Position

    final val Zero: Position = Raw(0, 0, 0, 0)
    final val One: Position = Raw(1, 1, 1, 1)
  }

  trait Position { self =>
    def x: Double
    def y: Double
    def z: Double
    def e: Double

    def +(other: Position) =
      Position(x + other.x, y + other.y, z + other.z, e + other.e)

    def -(other: Position) =
      Position(x - other.x, y - other.y, z - other.z, e - other.e)

    def *(scalar: Double): Position =
      Position(x * scalar, y * scalar, z * scalar, e * scalar)

    def *(other: Position): Double =
      x * other.x + y * other.y + z * other.z + e * other.e

    def /(scalar: Double) = this * (1.0f / scalar)

    lazy val length =
      //math.sqrt(x * x + y * y + z * z).toFP
      math.sqrt(x * x + y * y + z * z + e * e)

    lazy val invLength = 1.0f / length

    lazy val normal =
      if(length == 0) Position.Zero
      else this * invLength

    lazy val abs =
      Position(math.abs(x), math.abs(y), math.abs(z), math.abs(e))
  }

  sealed trait MathFault extends Throwable with NoStackTrace {
    override def toString() = getClass.getSimpleName
  }

  sealed trait EaseLimit extends MathFault

  case object PreEaseLimit extends EaseLimit
  case object PostEaseLimit extends EaseLimit

  case object LookaheadHalt extends MathFault
}

