package com.colingodsey.stepd.planner

import Math._

import scala.util.control.NoStackTrace


//takes move deltas, and produces iterable positions
trait PhysicsProcessor {
  def acc: Accel
  def jerk: Jerk

  //TODO: redo this to stash next G command period?
  var lastDelta = MoveDelta.Empty
  var curDelta = MoveDelta.Empty

  def recordFault(fault: MathFault): Unit

  def processTrapezoid(trap: Trapezoid): Unit

  def maxResizes = 15

  def popDelta(nextDelta: MoveDelta): Unit = {
    lastDelta = curDelta
    curDelta = nextDelta
  }

  def flush(): Unit = {
    processDelta(MoveDelta.Empty)
    processDelta(MoveDelta.Empty)
    processDelta(MoveDelta.Empty)
  }

  //TODO: add check where moveDelta can predict if post delta will have to slow down?
  //this can actually probably be done just on the moveDelta level?

  //TODO: keep last trap, use its end rate as the start rate for the next trap?
  ///^^^^^^^^^^^^^^^^^^^^^^^

  def willStartFrCauseResize(startFr: Double, post: MoveDelta): Boolean = {
    val dfr = post.f - startFr
    val accel = post.d.abs.normal * acc

    val accelTime = if(accel == 0) 0.0 else dfr / accel
    val accelDist = accel * accelTime * accelTime * 0.5 + startFr * accelTime

    post.isValid && accelDist >= (post.length * 0.5)
  }

  //TODO: is there any way to do a resize that allows a start *deccel* instead of accel?
  def processTrapezoid(pre: MoveDelta, moveDelta: MoveDelta, post: MoveDelta): Unit = {
    val dvStart = moveDelta.v - pre.v
    val frMaxStart = math.min(moveDelta.f, pre.f)
    val frStart = if(pre.isValid) frMaxStart * {
      val f = pre.d.normal * moveDelta.d.normal

      if(dvStart.abs * jerk.normal < jerk.length) 1.0
      else clamp(0.0, f, 1.0)
    } else 0.0

    val frAccel = moveDelta.d.abs.normal * acc

    val dvEnd = post.v - moveDelta.v
    val frMaxEnd = math.min(moveDelta.f, post.f)
    val frEnd = if(post.isValid) frMaxEnd * {
      val f = moveDelta.d.normal * post.d.normal

      if(dvEnd.abs * jerk.normal < jerk.length) 1.0
      else clamp(0.0, f, 1.0)
    } else 0.0

    val frDeccel = -frAccel

    if(willStartFrCauseResize(frEnd, post)) throw LookaheadHalt

    require(frAccel >= 0)
    require(frDeccel <= 0)

    val trap = Trapezoid(frStart, frAccel, moveDelta, frDeccel, frEnd)

    processTrapezoid(trap)
  }

  def processTrapezoidSafe(pre: MoveDelta, moveDelta: MoveDelta, post: MoveDelta, maxTimes: Int = maxResizes): Unit = {
    if(!moveDelta.isValid) return

    try processTrapezoid(pre, moveDelta, post) catch {
      case x: EaseLimit if maxTimes == 0 =>
        sys.error("Failed reducing trapezoid for acceleration")
        recordFault(x)
      case x: EaseLimit =>
        val newDelta = moveDelta.scaleFr(0.5)

        if(maxTimes == maxResizes) recordFault(x)
        processTrapezoidSafe(pre, newDelta, post, maxTimes - 1)
    }
  }

  def processDelta(nextDelta: MoveDelta): Unit = {
    try {
      processTrapezoidSafe(lastDelta, curDelta, nextDelta)
      popDelta(nextDelta)
    } catch {
      case LookaheadHalt =>
        recordFault(LookaheadHalt)

        require(nextDelta.f > 0.001, "unable to handle LookaheadHalt")

        processDelta(nextDelta.scaleFr(0.5))

        /*

        /* add an extra empty delta here. We know the next trap will bust, but the next
        trap doesnt know that we brought this one to a halt, so add an actual empty move */
        processDelta(MoveDelta.Empty)
        processDelta(nextDelta)

        */
    }
  }
}
