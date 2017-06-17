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

import scala.util.control.NoStackTrace

object Math {
  /*final val sqrtPi = math.sqrt(math.Pi)
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
  }*/

  @inline final def clamp(min: Double, x: Double, max: Double): Double =
    math.min(math.max(x, min), max)

  sealed trait MathFault extends Throwable with NoStackTrace {
    val simpleName = {
      val str = getClass.getSimpleName

      str.take(str.length - 1)
    }

    override def toString() = simpleName
  }

  object Vector4D {
    @inline def apply(x: Double, y: Double, z: Double, e: Double): Vector4D =
      Raw(x, y, z, e)

    def apply(seq: Int => Double): Vector4D =
      Raw(seq(0), seq(1), seq(2), seq(3))

    private final case class Raw(x: Double, y: Double, z: Double, e: Double) extends Vector4D

    final val Zero: Vector4D = Raw(0, 0, 0, 0)
    final val One: Vector4D = Raw(1, 1, 1, 1)
  }

  trait Vector4D { self: Equals =>
    def x: Double
    def y: Double
    def z: Double
    def e: Double

    lazy val length =
      math.sqrt(x * x + y * y + z * z + e * e)

    lazy val invLength = 1.0 / length

    lazy val normal =
      if(length == 0) Vector4D.Zero
      else this * invLength

    lazy val abs =
      Vector4D(math.abs(x),  math.abs(y),  math.abs(z),  math.abs(e))

    def +(other: Vector4D) =
      Vector4D(x + other.x,  y + other.y,  z + other.z,  e + other.e)

    def -(other: Vector4D) =
      Vector4D(x - other.x,  y - other.y,  z - other.z,  e - other.e)

    def *(other: Vector4D): Double =
               x * other.x + y * other.y + z * other.z + e * other.e

    def *(scalar: Double): Vector4D =
      Vector4D(x * scalar,   y * scalar,   z * scalar,   e * scalar)

    def /(scalar: Double) = this * (1.0f / scalar)

    override def canEqual(that: Any): Boolean = that match {
      case _: Vector4D => true
      case _ => false
    }

    override def equals(that: Any): Boolean = that match {
      case that: Vector4D =>
        x == that.x && y == that.y && z == that.z && e == that.e
      case _ =>
        false
    }
  }

  sealed trait EaseLimit extends MathFault

  case object PreEaseLimit extends EaseLimit
  case object PostEaseLimit extends EaseLimit

  case object LookaheadFault extends MathFault
}

