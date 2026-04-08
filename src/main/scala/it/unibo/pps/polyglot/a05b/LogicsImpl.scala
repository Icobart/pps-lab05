package it.unibo.pps.polyglot.a05b

import it.unibo.pps.polyglot.a05b.Logics

import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a05b/sol2/ */
class LogicsImpl(private val size: Int) extends Logics:

  private val random = Random()
  private val initialX = random.nextInt(size - 2) + 1
  private val initialY = random.nextInt(size - 2) + 1
  private var tickCount = 0

  override def tick(): Unit = tickCount += 1

  override def isOver: Boolean =
    initialX - tickCount < 0 || initialX + tickCount >= size ||
    initialY - tickCount < 0 || initialY + tickCount >= size

  override def hasElement(x: Int, y: Int): Boolean =
    val dx = (x - initialX).abs
    val dy = (y - initialY).abs
    (x == initialX && dy <= tickCount) ||
    (y == initialY && dx <= tickCount) ||
    (x - y == initialX - initialY && dx <= tickCount) ||
    (x + y == initialX + initialY && dx <= tickCount)

