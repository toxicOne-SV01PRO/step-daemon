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

import org.apache.commons.math3.analysis.interpolation.{BicubicInterpolator, PiecewiseBicubicSplineInterpolator}
import json._

object MeshLeveling {
  @accessor case class Point(x: Double, y: Double, offset: Double)

  class Reader(data: Array[Float], val width: Int, val height: Int) extends MeshLevelingReader {
    def getOffset(x: Double, y: Double): Float = {
      val xi = x.toInt
      val yi = y.toInt

      val isLastX = xi + 1 >= width
      val isLastY = yi + 1 >= height

      val q11 =
        data(xi        +       yi * width)

      val q21 = if(isLastX) q11
      else data(xi + 1 +       yi * width)

      val q12 = if(isLastY) q11
      else data(xi     + (yi + 1) * width)

      val q22 = if(isLastX || isLastY) q11
      else data(xi + 1 + (yi + 1) * width)

      val dx1 = (x - xi).toFloat
      val dy1 = (y - yi).toFloat
      val dx2 = 1.0f - dx1
      val dy2 = 1.0f - dy1

      val fy1 = q11 * dx2 + q21 * dx1
      val fy2 = q12 * dx2 + q22 * dx1

      fy1 * dy2 + fy2 * dy1
    }
  }

  def parseLine(line: String): Option[MeshLeveling.Point] = {
    //Bed X: 179.000 Y: 20.000 Z: 0.135
    val lead = "Bed " //leaves: "X: 179.000 Y: 20.000 Z: 0.135"
    val ident = lead + "X:"

    if(!line.startsWith(ident)) return None

    require(line.startsWith(lead))

    val split = line.drop(lead.length).split(' ')

    val x = split(1).toDouble
    val y = split(3).toDouble
    val z = split(5).toDouble

    require(!x.isNaN && !y.isNaN && !z.isNaN)

    Some(MeshLeveling.Point(x, y, z))
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

    //must be a full mesh! every X point must have every corresponding Y point
    for {
      xi <- 0 until sortX.length
      yi <- 0 until sortY.length
    } {
      val x = sortX(xi)
      val y = sortY(yi)
      val point = pointMap.get(x, y).getOrElse(
        sys.error(s"missing mesh leveling point ($x, $y)"))

      values(xi)(yi) = point.offset
    }

    //the good interpolator requires min 5 points each
    if(sortX.length >= 5 && sortY.length >= 5) {
      val interpolator = new PiecewiseBicubicSplineInterpolator

      interpolator.interpolate(sortX.toArray, sortY.toArray, values)
    } else {
      val interpolator = new BicubicInterpolator

      //TODO: might explode due to https://issues.apache.org/jira/browse/MATH-1138
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
