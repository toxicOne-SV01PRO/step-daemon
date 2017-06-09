package com.colingodsey.stepd.planner

import utest._

object MeshLevelingSuite extends TestSuite {
  val testOutput = "Recv: Bed X: 179.000 Y: 20.000 Z: 0.135\n\n\nRecv: Bed X: 136.000 Y: 20.000 Z: 0.332\n\n\nRecv: Bed X: 93.000 Y: 20.000 Z: 0.317\n\n\nRecv: Bed X: 50.000 Y: 20.000 Z: 0.269\n\n\nRecv: Bed X: 50.000 Y: 65.000 Z: 0.170\n\n\nRecv: Bed X: 93.000 Y: 65.000 Z: 0.103\n\n\nRecv: Bed X: 136.000 Y: 65.000 Z: 0.041\n\n\nRecv: Bed X: 179.000 Y: 65.000 Z: -0.159\n\n\nRecv: Bed X: 179.000 Y: 110.000 Z: -0.263\n\n\nRecv: Bed X: 136.000 Y: 110.000 Z: -0.031\n\n\nRecv: Bed X: 93.000 Y: 110.000 Z: 0.082\n\n\nRecv: Bed X: 50.000 Y: 110.000 Z: 0.191\n\n\nRecv: Bed X: 50.000 Y: 155.000 Z: 0.202\n\n\nRecv: Bed X: 93.000 Y: 155.000 Z: 0.111\n\n\nRecv: Bed X: 136.000 Y: 155.000 Z: 0.060\n\n\nRecv: Bed X: 179.000 Y: 155.000 Z: -0.063"

  val lines = testOutput.split('\n').map(_.trim).filter(_.nonEmpty).toSeq

  val points = lines map parseLine

  val minZ = points.map(_.offset).min.toFloat
  val maxZ = points.map(_.offset).max.toFloat

  val tests = this {
    val leveling = new MeshLeveling(points, 200, 200)
    val reader = leveling.reader()

    println(points)
    println(leveling.produce().toSeq)

    println(reader.getOffset(10, 10))
    println(reader.getOffset(10.5f, 10.5f))

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

  def checkZ(z0: Double): Unit = {
    //3 digits
    val z = (z0 * 1000).toInt / 1000f

    assert(z >= minZ)
    assert(z <= maxZ)
  }

  def parseLine(line: String): MeshLeveling.Point = {
    val lead = "Recv: Bed " //leaves: "X: 179.000 Y: 20.000 Z: 0.135"

    require(line.startsWith(lead))

    val split = line.drop(lead.length).split(' ')

    val x = split(1).toDouble
    val y = split(3).toDouble
    val z = split(5).toDouble

    require(!x.isNaN && !y.isNaN && !z.isNaN)

    MeshLeveling.Point(x, y, z)
  }
}
