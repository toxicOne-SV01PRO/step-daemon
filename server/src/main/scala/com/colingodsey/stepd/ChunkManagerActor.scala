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
      // Ok -> Writing allowed when re-writing a failed page
      Ok -> Set(Ok, Free, Writing),
      Fail -> Set(Ok, Fail, Writing)
    )

    val ValidTransitions: Set[(PageState, PageState)] = for {
      (from, tos) <- TransitionMap.toSet
      to <- tos
    } yield from -> to

    val byId: Int => PageState = All.map(x => x.id -> x).toMap

    def isValidTransition(x: (PageState, PageState)): Boolean = ValidTransitions(x)
  }
  sealed abstract class PageState(val id: Int)

  val maxGCodeQueue = 4
  val retryCooldown = 30.millis

  val testFailure = false
}

class ChunkManagerActor(gcodeSerial: ActorRef, maxPages: Int) extends Actor with Stash with ActorLogging with Pipeline.Terminator {
  import ChunkManagerActor._

  //Queue items that may depend on a page being written
  val queue = mutable.Queue[AnyRef]()

  //This is the host-authority page state
  var pageStates = Map[Int, PageState]().withDefaultValue(PageState.Free)
  var pendingPages = Map[Int, Chunk]()
  var resendDeadline = Map[Int, Deadline]().withDefaultValue(Deadline.now)

  var pendingCommands = 0

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

  def addPage(x: Chunk) = {
    val page = nextFreePage

    queue += x
    pendingPages += page -> x
    writePage(page)
  }

  def writePageRaw(page: Int) = {
    val testCorrupt = testFailure && math.random() < 0.05
    gcodeSerial ! LineSerial.Bytes(pendingPages(page).produceBytes(page, testCorrupt))
  }

  def writePage(page: Int) = {
    require(pageStates(page) != PageState.Writing)

    // set write flag eagerly, can't transition back to free
    pageStates += page -> PageState.Writing

    log.debug("writing page {}", page)

    writePageRaw(page)
  }

  def resendFailedPages(): Unit = pageStates foreach {
    case (page, PageState.Fail) if resendDeadline(page).isOverdue() =>
      log.warning("Resending failed page {}", page)
      resendDeadline += page -> retryCooldown.fromNow
      writePageRaw(page)
    case _ =>
  }

  def sendGCodeCommand(cmd: Command): Unit = {
    gcodeSerial ! SerialGCode.Command(cmd.raw.line)

    pendingCommands += 1
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

  def updatePageState(page: Int, state: PageState): Unit = {
    val curState = pageStates(page)
    val trans = curState -> state

    if (PageState isValidTransition trans) {
      pageStates += page -> state

      if (state == PageState.Free)
        pendingPages -= page

      if (curState != state) {
        log.debug("page {} transitioned from {} to {}", page, curState, state)
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
      resendFailedPages()
      drainPending()
      // if (!shouldStashCommands)
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
  }

  def waitStart: Receive = {
    case LineSerial.Response(str) if str.startsWith("setup_complete") =>
      context become receive

      log info "starting"

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

    implicit def ec = context.dispatcher
    context.system.scheduler.scheduleWithFixedDelay(1.seconds, 5.seconds) { () =>
      //TEST!!!!
      log.info(s"pageAvailable $pageAvailable")
      log.info(s"waitingCommands $waitingCommands")
    }

    context become waitStart
  }
}
