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
  val maxGCodeQueue = 4
  val retryCooldown = 30.millis

  val testFailure = false

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

  case object HealthCheck
}

class ChunkManagerActor(gcodeSerial: ActorRef, maxPages: Int) extends Actor with Stash
    with ActorLogging with Pipeline.Terminator with Timers {
  import ChunkManagerActor._
  import context.dispatcher

  //Queue items that may depend on a page being written
  val queue = mutable.Queue[AnyRef]()

  //This is the host-authority page state
  var pageStates = Map[Int, PageState]().withDefaultValue(PageState.Free)
  var pendingPages = Map[Int, Chunk]()
  var lastSent = Deadline.now

  var pendingCommands = 0

  //TODO: needs a guard that watches for potentially, like blocking transmit but still in fail

  // the queue is blocked waiting on a needed chunk transfer
  def canTransmitNext = queue.headOption match {
    case Some(x: Chunk) => getPageState(x) == PageState.Ok
    case None => false
    case _ => true
  }

  def pageAvailable = pageStates.exists(_._2 == PageState.Free)

  def waitingCommands = pendingCommands >= maxGCodeQueue

  def shouldStashCommands = !pageAvailable || waitingCommands

  def nextFreePage = {
    require(pageAvailable)

    pageStates.filter(_._2 == PageState.Free).head._1
  }

  def getPageId(x: Chunk) =
    pendingPages.filter(_._2 == x).headOption match {
      case Some((id, _)) => id
      case None => sys.error("chunk was not queued yet!")
    }

  def getPageState(x: Chunk): PageState = pageStates(getPageId(x))

  def writePage(page: Int) = {
    require(pageStates(page) != PageState.Writing)

    // set write flag eagerly as we can't transition back to free
    pageStates += page -> PageState.Writing

    log.debug("writing page {}", page)

    val testCorrupt = testFailure && math.random() < 0.05
    gcodeSerial ! LineSerial.Bytes(pendingPages(page).produceBytes(page, testCorrupt))
  }

  def addPage(x: Chunk) = {
    val page = nextFreePage

    require(!pendingPages.contains(page), "trying to replace pending page!")

    queue += x
    pendingPages += page -> x
    writePage(page)
  }

  def sendGCodeCommand(cmd: Command): Unit = {
    gcodeSerial ! SerialGCode.Command(cmd.raw.line)

    pendingCommands += 1
    lastSent = Deadline.now
  }

  def drainPending(): Unit = while (canTransmitNext) queue.removeHead() match {
    case x: Chunk =>
      val page = getPageId(x)
      log.debug("G6 for {}", page)
      sendGCodeCommand(GDirectMove(page))
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

      if (curState != state) {
        log.debug("page {} transitioned from {} to {}", page, curState, state)
      }

      if (trans == (PageState.Ok, PageState.Free))
        pendingPages -= page

      if (trans == (PageState.Fail, PageState.Free)) {
        log.debug("Resending failed page {}", page)
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

    val testCorruption = if (testFailure && math.random() < 0.05) 1 else 0

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

  def receive = {
    case LineSerial.Response(str) if str.startsWith("ok N") =>
      log.info("ok: {}", str)
    case LineSerial.Response(str) =>
      log.info("recv: {}", str)

    case LineSerial.ControlResponse(bytes) =>
      updatePageState(bytes)
      drainPending()
      unstashAll()

    //waiting on some free pages
    case _: Chunk | _: Command if shouldStashCommands =>
      stash()

    case x: Chunk =>
      ack()

      log debug "adding chunk"
      addPage(x)

    case x: Command =>
      ack()
      queue += x

    case _: StepProcessor.SyncPos =>
      ack()

    case SerialGCode.Completed(cmd) =>
      require(pendingCommands > 0, "more oks than sent commands!")
      pendingCommands -= 1

      log.debug("ok: {}", cmd)

      unstashAll()
      drainPending()

    case HealthCheck if shouldStashCommands =>
      def pendingPageId = queue.headOption match {
        case Some(x: Chunk) => Some(getPageId(x))
        case _ => None
      }

      if ((Deadline.now - lastSent) > 2.seconds) {
        log.error(s"Pipeline has stalled out! " +
          s"pageAvailable: $pageAvailable waitingCommands: $waitingCommands\n" +
          s"queueSize: ${queue.size} pendingPageId: $pendingPageId\n " +
          s"pageStates: $pageStates")
        context.parent ! PoisonPill
      }
  }

  def waitStart: Receive = {
    case LineSerial.Response(str) if str.startsWith("setup_complete") =>
      context become receive

      timers.startTimerAtFixedRate(self, HealthCheck, 1.second)

      log.info("starting " + self.path.toStringWithoutAddress)

      unstashAll()
    case LineSerial.Response(str) =>
      log.info("recv: {}", str)
    case _ =>
      stash()
  }

  override def preStart(): Unit = {
    super.preStart()

    context.system.eventStream.subscribe(self, classOf[LineSerial.Response])
    context.system.eventStream.subscribe(self, classOf[LineSerial.ControlResponse])

    context become waitStart
  }
}
