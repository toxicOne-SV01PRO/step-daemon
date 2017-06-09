package com.colingodsey.print3d.debug

import java.util.Scanner
import javafx.animation.AnimationTimer
import javafx.application.Application
import javafx.event.ActionEvent
import javafx.event.EventHandler
import javafx.scene.Scene
import javafx.scene.control.Button
import javafx.scene.control.Label
import javafx.scene.layout.StackPane
import javafx.scene.paint.Color
import javafx.scene.shape.Circle
import javafx.stage.Stage

import akka.actor._
import akka.util.ByteString
import com.colingodsey.print3d._
import com.colingodsey.stepd.CommandParser.SetPos
import com.colingodsey.stepd.Parser.GCodeCommand
import com.colingodsey.stepd.{CommandParser, LineSerial, Parser, SerialGCode}
import com.colingodsey.stepd.planner.Math.{Accel, Jerk, Position}
import com.colingodsey.stepd.planner._

import scala.concurrent.Await
import scala.concurrent.duration._

class UI extends Application {
  var t = 0.0
  var lastDeadline = Deadline.now
  var trapIdx = 0
  var lastPos = Position.Zero

  val speedScale = 0.5

  val root = new StackPane
  val scene = new Scene(root, 800, 600)
  val circle = new Circle()
  val timer = new AnimationTimer {
    def handle(now: Long): Unit = {
      val now = Deadline.now
      val dt = (now - lastDeadline).toMillis / 1000.0

      var pos = lastPos

      if(MovementProcessor.f != null) {
        MovementProcessor.f(dt * speedScale) foreach { x =>
          //if(math.random < 0.05) println(x)
          pos = x
        }
      }

      render(pos, dt)

      lastDeadline = now
    }
  }

  root.getChildren.add(circle)

  override def start(primaryStage: Stage): Unit = {
    primaryStage.setTitle("Planner Debug")

    primaryStage.setScene(scene)
    primaryStage.show()

    timer.start()

    circle.setRadius(5)
  }

  def render(pos: Position, dt: Double): Unit = {
    val v = (pos - lastPos).length / dt
    val c = ((v / 200.0) * (1024 + 256) / speedScale).toInt
    val r = math.min(c, 255)
    val g = math.max(math.min(c - 255, 255), 0)
    val b = math.max(math.min(c - 1024, 255), 0)

    circle.setFill(Color.rgb(r - b, g, b))
    circle.setTranslateX((pos.x - 100) * 20)
    circle.setTranslateY((pos.y - 100) * 20)

    lastPos = pos
  }
}

object UI extends App {
  //val acc: Accel = Position(2000, 1500, 100, 10000) //current settings
  //val jerk: Jerk = Position(15, 10, 0.4f, 5)

  val acc: Accel = Position(1500, 1000, 100, 10000)
  val jerk: Jerk = Position(7, 5, 0.2, 2.5)

  val stepSettings = new StepProcessor.StepSettings {
    val perUnit: com.colingodsey.stepd.planner.Math.Accel = Position(80, 80, 1600, 95.2f)

    //val ticksPerSecond: Double = 10000
    val ticksPerSecond: Double = 10000
  }

  val meshLeveling = {
    import MeshLeveling.Point

    val points = Seq(Point(179.0,20.0,0.135), Point(136.0,20.0,0.332), Point(93.0,20.0,0.317), Point(50.0,20.0,0.269), Point(50.0,65.0,0.17), Point(93.0,65.0,0.103), Point(136.0,65.0,0.041), Point(179.0,65.0,-0.159), Point(179.0,110.0,-0.263), Point(136.0,110.0,-0.031), Point(93.0,110.0,0.082), Point(50.0,110.0,0.191), Point(50.0,155.0,0.202), Point(93.0,155.0,0.111), Point(136.0,155.0,0.06), Point(179.0,155.0,-0.063))

    new MeshLeveling(points, 200, 200)
  }

  val system = ActorSystem()

  val movement = system.actorOf(Props(classOf[MovementProcessor]), name="motion-ui")
  val steps = system.actorOf(Props(classOf[StepProcessorActor], movement, stepSettings, meshLeveling.reader()), name="steps")
  val physics = system.actorOf(Props(classOf[PhysicsProcessorActor], steps, acc, jerk), name="physics")

  /*val movement = system.actorOf(Props(classOf[MovementProcessorPos]), name="motion-ui")
  val physics = system.actorOf(Props(classOf[PhysicsProcessorActor], movement, acc, jerk), name="physics")*/

  val delta = system.actorOf(Props(classOf[DeltaProcessorActor], physics, true), name="delta")
  val commands = system.actorOf(Props(classOf[CommandStreamer], delta), name="commands")

  val serial = system.actorOf(Props(classOf[LineSerial]), name="print-serial")
  val gcodeSerial = system.actorOf(Props(classOf[SerialGCode], serial), name="gcode-serial")

  //val serialTest = system.actorOf(Props(classOf[SerialTest], gcodeSerial, serial), name="serial-test")

  sys.addShutdownHook {
    system.terminate()

    Await.result(system.whenTerminated, 10.seconds)
  }

  Application.launch(classOf[UI], args: _*)

  /*import system.dispatcher

  system.scheduler.schedule(5.seconds, 10.millis) {
    if(MovementProcessor.f != null) {
      MovementProcessor.f(10.0 / 1000)
    }
  }*/
}

class SerialTest(gcodeSerial: ActorRef, rawSerial: ActorRef) extends Actor with ActorLogging {
  import context.dispatcher

  val testCode = SerialGCode.Command("M105")

  var started = false
  var bytesSent = 0
  var lastTime = Deadline.now
  var pendingChunks = 0
  var codesSent = 0

  context.system.scheduler.schedule(10.seconds, 10.second, self, Report)

  gcodeSerial ! LineSerial.Subscribe

  def testChunk = {
    val bytes = (0 until 256).map(_ => (math.random * 256).toByte)
    //val bytes = (0 until 256).map(_ => 'A'.toByte)

    Chunk(ByteString.empty ++ bytes)
  }

  def sendChunk(nDone: Int): Unit = {
    pendingChunks -= nDone

    while(pendingChunks < 1) {
      val chunk = testChunk

      bytesSent += chunk.length

      rawSerial ! LineSerial.Bytes(chunk.chunk)

      pendingChunks += 1
    }
  }

  def receive = {
    case LineSerial.Response("looptest") =>
      scala.concurrent.blocking(Thread.sleep(5000))
      started = true
      log.info("started")
      sendChunk(0)
      gcodeSerial ! testCode
    case LineSerial.Response(str) if str.startsWith("!busy") =>
      log.debug("chunk {}", str)
      sendChunk(1)
    case LineSerial.Response(str) if str.startsWith("!ok") =>
      log.debug("chunk {}", str)
      sendChunk(1)
    case LineSerial.Response(str) if str.startsWith("!fail") =>
      log.info("chunk {}", str)
      sendChunk(1)
    case LineSerial.Response(str) =>
      log.info("recv: {}", str)
    case SerialGCode.Completed(`testCode`) =>
      gcodeSerial ! testCode
      codesSent += 1
    case Report =>
      val now = Deadline.now
      val dt = now - lastTime
      val dts = dt.toMillis / 1000.0
      val bps = bytesSent / dts * 8
      val cps = codesSent / dts

      log.info("bps {} codes {}", bps.toInt, cps.toInt)

      gcodeSerial ! SerialGCode.Command("M114")

      lastTime = now
      bytesSent = 0
      codesSent = 0
  }

  object Report
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

  def sendSome(): Unit = {
    for(_ <- 0 until chunkSize) {
      if(!itr.hasNext) {
        //log.info("restarting...")
        itr = content.iterator
      }

      process(itr.next())
    }

  }

  def receive: Receive = pipeline orElse {
    case Process =>
      sendSome()

      //log.info("some")

      self ! Process
  }

  object Process
}

object MovementProcessor {
  var f: Double => Option[Position] = null
}

class MovementProcessor extends Actor with Stash with ActorLogging {
  import Math._

  val stepsPer = UI.stepSettings.perUnit

  var ticks = 0.0
  var idx = 0
  var curChunk: ByteString = null

  var x = 0L
  var y = 0L
  var z = 0L
  var e = 0L

  @volatile var pos = Position.Zero

  MovementProcessor.f = getPos(_)

  def takeStep(): Unit = {
    val blockIdx = (idx >> 3) << 1
    val stepIdx = idx & 0x7

    val a = curChunk(blockIdx) & 0xFF
    val b = curChunk(blockIdx + 1) & 0xFF

    val xRaw = (a & 0xF0) >> 4
    val yRaw = a & 0xF
    val zRaw = (b & 0xF0) >> 4
    val eRaw = b & 0xF

    x += xRaw - 7
    y += yRaw - 7
    z += zRaw - 7
    e += eRaw - 7

    pos = Position(x / stepsPer.x, y / stepsPer.y, z / stepsPer.z, e / stepsPer.e)

    //idx += 1
    idx += 8

    if(idx == StepProcessor.StepsPerChunk) {
      idx = 0
      curChunk = null

      unstashAll()
    }
  }

  def getPos(dt: Double): Option[Position] = {
    val x = pos

    self ! Tick(dt * UI.stepSettings.ticksPerSecond / StepProcessor.StepsPerBlock)

    Some(x)
  }

  def consumeBuffer(): Unit = {
    while(ticks >= 1 && curChunk != null) {
      takeStep()

      ticks -= 1
    }
  }

  def receive: Receive = {
    case buff: ByteString if curChunk == null =>
      sender ! Pipeline.Ack

      curChunk = buff

      consumeBuffer()
    case Tick(x) =>
      ticks += x

      consumeBuffer()
    case _: ByteString =>
      stash()
    case setPos: SetPos if curChunk == null =>
      x = (setPos.x.getOrElse(pos.x) * stepsPer.x).round
      y = (setPos.y.getOrElse(pos.y) * stepsPer.y).round
      z = (setPos.z.getOrElse(pos.z) * stepsPer.z).round
      e = (setPos.e.getOrElse(pos.e) * stepsPer.e).round

      sender ! Pipeline.Ack
    case setPos: SetPos =>
      stash()
    case _: GCodeCommand =>
      sender ! Pipeline.Ack
  }

  case class Tick(ticks: Double)
}

class MovementProcessorPos extends Actor with Stash with ActorLogging {
  import Math._

  var t = 0.0
  var trap: Trapezoid = null

  MovementProcessor.f = getPos(_)

  def processTrap(dt: Double): Position = synchronized {
    val d = trap.getPos(t)

    val pos = trap.move.from + trap.move.d.normal * d

    t += dt

    if(t >= trap.time) {
      t -= trap.time

      trap = null

      self ! Unstash
    }

    pos
  }

  def getPos(dt: Double) = synchronized {
    if(trap == null) {
      t += dt
      None
    } else Some(processTrap(dt))
  }

  def receive: Receive = {
    case x: Trapezoid if trap == null =>
      synchronized {
        trap = x
        processTrap(0.0)
      }

      sender ! Pipeline.Ack
    case trap: Trapezoid =>
      stash()
    case Unstash =>
      unstashAll()
    case _: GCodeCommand =>
      sender ! Pipeline.Ack
  }

  object Unstash
}
