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

import akka.actor._
import akka.util.ByteString
import com.colingodsey.stepd.GCode._
import com.colingodsey.stepd.planner._
import com.colingodsey.stepd.serial.{LineSerial, SerialGCode}

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration._

/*
New manager:
Keep map of pending buffers.
Keep map of client state.

Blah, or fancy style:

Keep actor for each page.
Child map kept for each page.
Child is loaded for each transfer.
Child remains until transfer is ready.
Child dies, completes responses, and is removed from map.

Any new incoming chunk will block the stream until a new remote chunk is available.

The gcode-stream at this actor lags far behind the actual action. This lends well to the mechanics for page buffering.

The key is that the gcode 'action' wont be written until the page is confirmed. And the gcode stream lags behind anyways...

Chunks will need to be converted to 'chunk references' or something, and block if its not confirmed.

Manager will need a queue. Everything in the queue will have a chance to buffer.
Only items in the queue that are buffered will be sent to the serial actor.

def chunk -> gcode_c(ActorRef, chunk). ActorRef could be Future for deathwatch?


 */

object ChunkManagerActor {
  val MaxGCodeQueue = 4
  val RetryCooldown = 30.millis
  val NumPages = 16

  val TestFailure = false

  object PageState {
    case object Free extends PageState(0)
    case object Writing extends PageState(1)
    case object Ok extends PageState(2)
    case object Fail extends PageState(3)

    val All = Set[PageState](Free, Writing, Ok, Fail)

    //TODO we need a g-code to 'unlock' failed pages

    //allowed transitions for client-provided state
    val TransitionMap = Map[PageState, Set[PageState]](
      Free -> Set(Free),
      // concurrency may produce false "free" when we start writing
      Writing -> Set(Ok, Fail, Writing),
      Ok -> Set(Ok, Free),
      Fail -> Set(Fail, Free)
    )

    val ValidTransitions: Set[(PageState, PageState)] = for {
      (from, tos) <- TransitionMap.toSet
      to <- tos
    } yield from -> to

    val byId: Int => PageState = All.map(x => x.id -> x).toMap

    def isValidTransition(x: (PageState, PageState)): Boolean = ValidTransitions(x)
  }
  sealed abstract class PageState(val id: Int)

  case class StagedChunk(page: Int)

  case object HealthCheck
  case object Stats
}

class ChunkManagerActor(gcodeSerial: ActorRef, maxPages: Int) extends Actor with Stash
    with ActorLogging with Pipeline.Terminator with Timers {
  import ChunkManagerActor._
  import context.dispatcher

  //Queue items that may depend on a page being written
  val queue = mutable.Queue[AnyRef]()

  //This is the host-authority page state
  var pageStates = Map[Int, PageState]()
  var pageChunks = Map[Int, Chunk]()
  var lastSent = Deadline.now

  var pendingCommands = 0
  var sentChunkBytes = 0
  var nextFreePage = 0

  var lastReportedSpeed = 0
  var lastReportedDirection = Seq(false, false, false, false)
  var hasReportedDirection = false

  //TODO: needs a guard that watches for a page blocking transmit if in fail state

  def pageAvailable = pageStates(nextFreePage) == PageState.Free

  def waitingCommands = pendingCommands >= MaxGCodeQueue

  def shouldStashCommands = !pageAvailable || waitingCommands

  // the queue is blocked waiting on a needed chunk transfer
  def canTransmitNext = queue.headOption match {
    case Some(StagedChunk(page)) => pageStates(page) == PageState.Ok
    case Some(_: Command) => !waitingCommands
    case Some(_) => sys.error("something random in our queue")
    case None => false
  }

  def writePage(page: Int) = {
    require(pageStates(page) != PageState.Writing)

    // set write flag eagerly as we can't transition back to free
    pageStates += page -> PageState.Writing

    log.debug("writing page {}", page)

    val testCorrupt = TestFailure && math.random() < 0.05

    val chunk = pageChunks(page)

    //log.info("{} {}", chunk.rawBytes.length, chunk.meta)

    sentChunkBytes += chunk.rawBytes.length

    gcodeSerial ! LineSerial.Bytes(chunk.produceBytes(page, testCorrupt))
  }

  def addPage(x: Chunk) = {
    val page = nextFreePage

    nextFreePage += 1
    if (nextFreePage >= NumPages) nextFreePage = 0

    require(!pageChunks.contains(page), s"trying to replace pending page $page")

    queue += StagedChunk(page)
    pageChunks += page -> x
    writePage(page)
  }

  def sendGCodeCommand(cmd: Command): Unit = {
    gcodeSerial ! SerialGCode.Command(cmd.raw.line)

    pendingCommands += 1
    lastSent = Deadline.now
  }

  def getDirectionBit(axis: Int, meta: StepProcessor.ChunkMeta) =
    if (lastReportedDirection(axis) != meta.directions(axis) || !hasReportedDirection)
      Some(meta.directions(axis))
    else None

  def drainPending(): Unit = while (canTransmitNext) queue.removeHead() match {
    case StagedChunk(page) =>
      val chunk = pageChunks(page)

      val speed = if (lastReportedSpeed != chunk.meta.speed) {
        lastReportedSpeed = chunk.meta.speed
        Some(lastReportedSpeed)
      } else None

      val xDir = getDirectionBit(0, chunk.meta)
      val yDir = getDirectionBit(1, chunk.meta)
      val zDir = getDirectionBit(2, chunk.meta)
      val eDir = getDirectionBit(3, chunk.meta)

      hasReportedDirection = true
      lastReportedDirection = chunk.meta.directions

      val move = GDirectMove(
        index=Some(page),
        speed=speed,
        steps=chunk.meta.steps,
        xDir=xDir,
        yDir=yDir,
        zDir=zDir,
        eDir=eDir
      )

      log.debug("G6 for {}", page)
      sendGCodeCommand(move)
    case x: Command =>
      sendGCodeCommand(x)
    case x => sys.error("unexpected queued item " + x)
  }

  def sendPageUnlock(page: Int) = {
    val bytes = Chunk.getUnlockPageBytes(page)
    gcodeSerial ! LineSerial.Bytes(bytes)
  }

  def updatePageState(page: Int, state: PageState): Unit = {
    val curState = pageStates(page)
    val trans = curState -> state

    if (PageState isValidTransition trans) {
      pageStates += page -> state

      if (curState != state)
        log.debug("page {} transitioned from {} to {}", page, curState, state)

      if (trans == (PageState.Ok, PageState.Free))
        pageChunks -= page

      if (trans == (PageState.Fail, PageState.Free)) {
        log.debug("Failed page {} unlocked", page)
        writePage(page)
      }

      if (trans == (PageState.Writing, PageState.Fail)) {
        log.warning("Page {} failed to write", page)
        sendPageUnlock(page)
      }
    } else if (trans != (PageState.Writing, PageState.Free)) {
      log.warning("Bad state transition for {}: {}", page, trans)
    }
  }

  def updatePageState(bytes: ByteString): Unit = {
    require(bytes.length == 5)

    val testCorruption = if (TestFailure && math.random() < 0.05) 1 else 0

    val checksum = bytes(4)
    val crc = (bytes.slice(0, 4).foldLeft(0)(_ ^ _) & 0xFF).toByte + testCorruption.toByte

    if (crc != checksum) {
      log.warning("Failed checksum on control message: expected {} got {}", checksum, crc)
    } else for {
      pageIdx <- 0 until 16
      byteIdx = pageIdx / 4
      bitIdx = (pageIdx * 2) % 8
      stateId = (bytes(byteIdx) >>> bitIdx) & 3
      state = PageState byId stateId
    } updatePageState(pageIdx, state)
  }

  /*
  TODO: need to start filling pages in numeric order, recognize fail state
  of getting written -> ok from a later page without getting one for the early page
   */

  def receive = {
    case LineSerial.Response(str) if str.startsWith("ok N") =>
      log.debug("ok: {}", str)
    case LineSerial.Response(str) =>
      log.info("recv: {}", str)

    case LineSerial.Response(str) if str.startsWith("pages_ready") =>
      log.error("Printer restarted!")
      context.parent ! PoisonPill

    case LineSerial.ControlResponse(bytes) =>
      updatePageState(bytes)
      drainPending()
      unstashAll()

    //waiting on some free pages
    case _: Chunk | _: Command if shouldStashCommands =>
      stash()

    case x: Chunk =>
      ack()
      addPage(x)

    case x: Command =>
      ack()
      queue += x
      drainPending()

    case _: StepProcessor.SyncPos =>
      ack()

    case SerialGCode.Completed(cmd) =>
      require(pendingCommands > 0, "more oks than sent commands!")
      pendingCommands -= 1

      log.debug("ok: {}", cmd)

      drainPending()
      unstashAll()

    case HealthCheck if shouldStashCommands =>
      //TODO: this fails when waiting on heating. hrm....
      /*if ((Deadline.now - lastSent) > 2.seconds) {
        log.error(s"Pipeline has stalled out! " + stateString)
        context.parent ! PoisonPill
      }*/

    case Stats =>
      val bytesPerSec = (sentChunkBytes * 1000.0 / 5.0).toInt / 1000.0

      sentChunkBytes = 0

      if (bytesPerSec > 0) {
        log.info("Chunk bytes/s: {}", bytesPerSec)
      }
  }

  def stateString = {
    def pendingPageId = queue.headOption match {
      case Some(StagedChunk(x)) => Some(x)
      case _ => None
    }

    s"pageAvailable: $pageAvailable waitingCommands: $waitingCommands\n" +
      s"queueSize: ${queue.size} pendingPageId: $pendingPageId\n" +
      s"pageStates: $pageStates"
  }


  def waitStart: Receive = {
    case LineSerial.Response(str) if str.startsWith("pages_ready") =>
      context become receive

      timers.startTimerAtFixedRate("health", HealthCheck, 1.second)
      timers.startTimerAtFixedRate("stats", Stats, 5.seconds)

      log.info("starting " + self.path.toStringWithoutAddress)

      for (i <- 0 until NumPages)
        pageStates += i -> PageState.Free

      unstashAll()
    case LineSerial.Response(str) =>
      log.info("recv: {}", str)
    case _ =>
      stash()
  }

  override def postStop(): Unit = {
    log.info("Stopping: " + stateString)
    super.postStop()
  }

  override def preStart(): Unit = {
    super.preStart()

    context.system.eventStream.subscribe(self, classOf[LineSerial.Response])
    context.system.eventStream.subscribe(self, classOf[LineSerial.ControlResponse])

    context become waitStart
  }
}
