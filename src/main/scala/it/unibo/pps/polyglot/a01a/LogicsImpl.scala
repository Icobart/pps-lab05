package it.unibo.pps.polyglot.a01a

import it.unibo.pps.polyglot.a01a.Logics
import it.unibo.pps.polyglot.a01a.Logics.Result
import it.unibo.pps.util.Sequences.Sequence

import scala.util.Random

trait ScalaLogics:
  def hit(row: Int, col: Int): Result

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01a/sol2/ */
class LogicsImpl(private val size: Int, private val boat: Int) extends Logics with ScalaLogics:

  private val random = Random()
  private val rowStart = random.nextInt(size)
  private val colStart = random.nextInt(size - boat + 1)
  private val maxMisses = 20
  private var hitCount = 0
  private var missCount = 0
  private var hits: Sequence[(Int, Int)] = Sequence.Nil()

  def hit(row: Int, col: Int): Result = {
    val isBoat = row == rowStart && col >= colStart && col < colStart + boat
    isBoat match
      case true if hits.contains((row, col)) => Result.HIT
      case true => hits = Sequence.Cons((row, col), hits)
        hitCount += 1
        if hitCount == boat then Result.WON else Result.HIT
      case false => missCount += 1
        if missCount >= maxMisses then Result.LOST else Result.MISS
  }
