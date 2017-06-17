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

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import utest._

object MeshLevelingSuite extends TestSuite {

  lazy val test4x4Points = {
    val testOutput = "Bed X: 179.000 Y: 20.000 Z: 0.135\n\n\nBed X: 136.000 Y: 20.000 Z: 0.332\n\n\nBed X: 93.000 Y: 20.000 Z: 0.317\n\n\nBed X: 50.000 Y: 20.000 Z: 0.269\n\n\nBed X: 50.000 Y: 65.000 Z: 0.170\n\n\nBed X: 93.000 Y: 65.000 Z: 0.103\n\n\nBed X: 136.000 Y: 65.000 Z: 0.041\n\n\nBed X: 179.000 Y: 65.000 Z: -0.159\n\n\nBed X: 179.000 Y: 110.000 Z: -0.263\n\n\nBed X: 136.000 Y: 110.000 Z: -0.031\n\n\nBed X: 93.000 Y: 110.000 Z: 0.082\n\n\nBed X: 50.000 Y: 110.000 Z: 0.191\n\n\nBed X: 50.000 Y: 155.000 Z: 0.202\n\n\nBed X: 93.000 Y: 155.000 Z: 0.111\n\n\nBed X: 136.000 Y: 155.000 Z: 0.060\n\n\nBed X: 179.000 Y: 155.000 Z: -0.063"

    val lines = testOutput.split('\n').map(_.trim).filter(_.nonEmpty).toSeq

    val points = lines map { line =>
      val pointOpt = MeshLeveling parseLine line

      require(pointOpt.isDefined, "failed parsing line")

      pointOpt.get
    }

    require(points.length == (4 * 4), "failed parsing all points")

    points
  }

  def produce4x4PNG(leveling: MeshLeveling): Unit = {
    val points = test4x4Points
    val minZ = points.map(_.offset).min.toFloat
    val maxZ = points.map(_.offset).max.toFloat

    val leveling = new MeshLeveling(points, 200, 200)
    val img = new BufferedImage(2000, 2000, BufferedImage.TYPE_INT_RGB)

    //val mesh = leveling.produce()
    val reader = leveling.reader()

    for {
      x <- 0 until 2000
      y <- 0 until 2000
    } {
      val z = reader.getOffset(x / 10.0, y / 10.0).toDouble

      val z0 = (z - minZ) / (maxZ - minZ)
      val totalSpace = 255 * 3
      val z1 = z0 * totalSpace

      val r = math.min(math.max(z1, 0), 255).toInt
      val g = math.min(math.max(z1 - 255, 0), 255).toInt
      val b = math.min(math.max(z1 - 255 * 2, 0), 255).toInt

      img.setRGB(x, y, new Color(r - b, g, b).getRGB)
    }

    ImageIO.write(img, "PNG", new File("./meshleveling.png"));
  }

  val tests = this {
    "img" - {
      val points = test4x4Points
      val leveling = new MeshLeveling(points, 200, 200)

      produce4x4PNG(leveling)
    }
    "4x4 points" - {
      val points = test4x4Points
      val minZ = points.map(_.offset).min.toFloat
      val maxZ = points.map(_.offset).max.toFloat
      val leveling = new MeshLeveling(points, 200, 200)
      val reader = leveling.reader()

      println(points)
      println(leveling.produce().toSeq)

      println(reader.getOffset(10, 10))
      println(reader.getOffset(10.5f, 10.5f))

      def checkZ(z0: Double): Unit = {
        //3 digits
        val z = (z0 * 1000).toInt / 1000f

        assert(z >= minZ)
        assert(z <= maxZ)
      }

      "check raw points" - {
        for(z <- leveling.produce())
          checkZ(z)
      }

      "check int points" - {
        for(x <- 0 until 199; y <- 0 until 199) {
          val z = reader.getOffset(x, y)

          checkZ(z)
        }
      }

      "check sub points" - {
        for {
          x <- 0 until 199
          y <- 0 until 199
          x2 <- 0 until 20
          y2 <- 0 until 20
        } {
          val z = reader.getOffset(x + x2 / 20.0f, y + y2 / 20.0f)

          checkZ(z)
        }
      }
    }
  }
}
