package com.colingodsey.stepd

import java.util.Scanner

import akka.actor._
import com.colingodsey.stepd.Parser.GCodeCommand
import com.colingodsey.stepd.planner.Math._
import com.colingodsey.stepd.planner._

import scala.concurrent.Await
import scala.concurrent.duration._

object Server extends App {

  //val acc: Accel = Position(2000, 1500, 100, 10000) //current settings
  //val jerk: Jerk = Position(15, 10, 0.4f, 5)

  //val acc: Accel = Position(1500, 1000, 100, 10000) //new ideal settings
  val acc: Accel = Position(1000, 700, 100, 10000) //new ideal settings
  val jerk: Jerk = Position(7, 5, 0.2, 2.5)

  val stepSettings = new StepProcessor.StepSettings {
    val perUnit: Position = Position(80, 80, 1600, 95.2f)

    val ticksPerSecond: Double = 30000
  }

  val meshLeveling = {
    import MeshLeveling.Point

    val points = Seq(Point(179.0,20.0,0.135), Point(136.0,20.0,0.332), Point(93.0,20.0,0.317), Point(50.0,20.0,0.269), Point(50.0,65.0,0.17), Point(93.0,65.0,0.103), Point(136.0,65.0,0.041), Point(179.0,65.0,-0.159), Point(179.0,110.0,-0.263), Point(136.0,110.0,-0.031), Point(93.0,110.0,0.082), Point(50.0,110.0,0.191), Point(50.0,155.0,0.202), Point(93.0,155.0,0.111), Point(136.0,155.0,0.06), Point(179.0,155.0,-0.063))

    new MeshLeveling(points, 200, 200)
  }

  val system = ActorSystem()

  val serial = system.actorOf(Props(classOf[LineSerial]), name="print-serial")
  val gcodeSerial = system.actorOf(Props(classOf[SerialGCode], serial), name="gcode-serial")

  val chunkManager = system.actorOf(Props(classOf[ChunkManagerActor], serial, gcodeSerial, 5), name="chunk-manager")

  val steps = system.actorOf(Props(classOf[StepProcessorActor], chunkManager, stepSettings, meshLeveling.reader()), name="steps")
  val physics = system.actorOf(Props(classOf[PhysicsProcessorActor], steps, acc, jerk), name="physics")

  val delta = system.actorOf(Props(classOf[DeltaProcessorActor], physics, false), name="delta")
  val commands = system.actorOf(Props(classOf[CommandStreamer], delta), name="commands")

  val m114Handler = system.actorOf(Props(classOf[M114Handler], delta, serial), name="m114-handler")

  //system.scheduler.schedule(15.seconds, 10.seconds, gcodeSerial, SerialGCode.Command("M114"))

  sys.addShutdownHook {
    system.terminate()

    Await.result(system.whenTerminated, 10.seconds)
  }
}

class CommandStreamer(val next: ActorRef) extends Pipeline with Parser with CommandParser with ActorLogging {
  //val stream = getClass.getResourceAsStream("/g_test1.gcode")
  val stream = getClass.getResourceAsStream("/xyzCalibration_cube.gcode")
  //val stream = getClass.getResourceAsStream("/test2.gcode")
  //val lines = scala.io.Source.fromInputStream(stream).getLines.mkString("\r\n")

  val content =
    new Scanner(stream).useDelimiter("\\A").next() + "\r\n"

  val chunkSize = 512

  var itr = content.iterator

  self ! Process

  def process(cmd: GCodeCommand): Unit = {
    sendDown(cmd)
  }

  def sendSome(): Boolean = {
    /*for(_ <- 0 until chunkSize) {
      if(!itr.hasNext) {
        log.info("restarting...")
        itr = content.iterator
      }

      process(itr.next())
    }*/

    for(_ <- 0 until chunkSize) {
      if(!itr.hasNext) {
        log.info("done.")
        //itr = content.iterator
      }

      if(itr.hasNext)
        process(itr.next())
    }

    itr.hasNext
  }

  def receive: Receive = pipeline orElse {
    case Process =>
      if(sendSome())
        self ! Process
  }

  object Process
}