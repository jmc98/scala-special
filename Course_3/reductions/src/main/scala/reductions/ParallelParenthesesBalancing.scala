package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` if the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def countDepth(pos: Int, depth: Int): Boolean =
      if (depth < 0) false
      else if (pos >= chars.length) depth == 0
      else if (chars(pos) == '(') countDepth(pos + 1, depth + 1)
      else if (chars(pos) == ')') countDepth(pos + 1, depth - 1)
      else countDepth(pos + 1, depth)

    countDepth(0, 0)
  }

  /** Returns `true` if the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int): (Int, Int) = {
      var minCount = 0
      var count = 0
      var i = idx
      while (i < until) {
        if (chars(idx) == '(') count += 1
        if (chars(idx) == ')') {
          count -= 1
          if (count < minCount) minCount = count
        }
        i += 1
      }
      (minCount, count)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) traverse(from, until)
      else {
        val mid = from + (until - from) / 2
        val ((leftMin, leftCount), (rightMin, rightCount)) = parallel(reduce(from, mid), reduce(mid, until))
        val rightCancel = math.max(0, leftCount - leftMin)
        val newLeftMin = leftMin + math.min(0, rightMin + rightCancel)
        (newLeftMin, leftCount + rightCount)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
