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

import org.apache.commons.math3.analysis.interpolation.{BicubicInterpolator, BivariateGridInterpolator, PiecewiseBicubicSplineInterpolator}

object MeshLeveling {
  case class Point(x: Double, y: Double, offset: Double)

  class Reader(data: Array[Float], val width: Int, val height: Int) {
    def getOffset(x: Double, y: Double): Double = {
      val xi = x.toInt
      val yi = y.toInt

      val isLastX = x + 1 >= width
      val isLastY = y + 1 >= height

      val q11 = data(xi + yi * width)

      val q21 = if(isLastX) q11
      else data(xi + 1 + yi * width)

      val q12 = if(isLastY) q11
      else data(xi + (yi + 1) * width)

      val q22 = if(isLastX || isLastY) q11
      else data(xi + 1 + (yi + 1) * width)

      val dx1 = x - xi
      val dy1 = y - yi
      val dx2 = 1.0f - dx1
      val dy2 = 1.0f - dy1

      val fy1 = q11 * dx2 + q21 * dx1
      val fy2 = q12 * dx2 + q22 * dx1

      fy1 * dy2 + fy2 * dy1
    }
  }
}

//TODO: this *REALLY* needs to run a self test, especially with the flaky BicubicInterpolator
class MeshLeveling(val points: Seq[MeshLeveling.Point], val width: Int, val height: Int) {
  import MeshLeveling._

  val maxX = points.iterator.map(_.x).max
  val minX = points.iterator.map(_.x).min
  val maxY = points.iterator.map(_.y).max
  val minY = points.iterator.map(_.y).min

  val function = {
    val sortX = points.map(_.x).distinct.sorted.toIndexedSeq
    val sortY = points.map(_.y).distinct.sorted.toIndexedSeq

    val pointMap = points.map(point => (point.x, point.y) -> point).toMap

    val values = Array.fill(sortX.length)(new Array[Double](sortY.length))

    for {
      xi <- 0 until sortX.length
      yi <- 0 until sortY.length
    } {
      val x = sortX(xi)
      val y = sortY(yi)
      val point = pointMap(x, y)

      values(xi)(yi) = point.offset
    }


    if(sortX.length >= 5) {
      val interpolator = new PiecewiseBicubicSplineInterpolator

      interpolator.interpolate(sortX.toArray, sortY.toArray, values)
    } else {
      val interpolator = new BicubicInterpolator

      interpolator.interpolate(sortX.toArray, sortY.toArray, values)
    }
  }

  def calculateFor(x0: Double, y0: Double): Double = {
    var x = x0
    var y = y0

    if(x < minX) x = minX
    if(x > maxX) x = maxX
    if(y < minY) y = minY
    if(y > maxY) y = maxY

    function.value(x, y)
  }

  def produce() = {
    val out = new Array[Float](width * height)

    for {
      x <- 0 until width
      y <- 0 until height
    } yield {
      val f = calculateFor(x, y)
      val idx = x + y * width

      out(idx) = f.toFloat
    }

    out
  }

  def reader() = new Reader(produce(), width, height)
}
