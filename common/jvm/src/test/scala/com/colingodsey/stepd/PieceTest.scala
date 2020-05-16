/*
 * Copyright 2020 Colin Godsey
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

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO
import utest._
import planner.Pieces._

import scala.util.control.NonFatal

object PieceTest extends TestSuite {
  def draw(shape: Trapezoid, name: String): Unit = {
    val nx = 400
    val dt = shape.dt
    val dx = dt / nx

    val img = new BufferedImage(nx, nx, BufferedImage.TYPE_INT_RGB)

    def int0vals = for (i <- 0 until nx) yield i -> shape(i * dx)
    def int1vals = for (i <- 0 until nx) yield i -> shape.int1At(i * dx)
    def int2vals = for (i <- 0 until nx) yield i -> shape.int2At(i * dx, 0)

    def drawPoints(vals: Seq[(Int, Double)], color: Int) = {
      val min = 0 //vals.map(_._2).min
      val max = vals.map(_._2).max

      for {
        (x, y0) <- vals
        y1 = (y0 - min) / (max - min)
        y2 = (1.0 - y1) * 0.95 + 0.025
        y = math.round(y2 * nx).toInt
        if y > 0 && y < nx
      } try {
        img.setRGB(x.toInt, y, color)
      } catch {
        case NonFatal(e) =>
          println((min, max))
          println(max - min)
          println(y)
          throw e
      }
    }

    drawPoints(int0vals, Color.GREEN.getRGB)
    drawPoints(int1vals, Color.RED.getRGB)
    drawPoints(int2vals, Color.YELLOW.getRGB)

    ImageIO.write(img, "PNG", new File(name))
  }

  val tests = Tests {
    test("vtrap test") {
      val shape = Trapezoid(
        Pulse(5, 10),
        Pulse(-5, -10),
        50
      )

      draw(shape, "vtrap-test.png")
    }

    test("atrap test") {
      val shape0 = Trapezoid(
        Trapezoid(
          Pulse(4, 5),
          Pulse(-4, -5),
          20
        ),
        Trapezoid(
          Pulse(4, 5),
          Pulse(-4, -5),
          20
        ),
        //Pulse(-5, -20),
        200
      )

      draw(shape0, "atrap-test.png")
    }
  }
}
