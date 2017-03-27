import java.util
import java.util.{Collections, Scanner}


class Solution {
  def indexByCount(sortedCounts: Array[Int], i: Int, maxIndex: Int) = {
    var indexOfCount = util.Arrays.binarySearch(sortedCounts, 0, maxIndex, i)
    if (indexOfCount > 0) {
      while (indexOfCount - 1 >= 0 && i.equals(sortedCounts(indexOfCount - 1)))
        indexOfCount = indexOfCount - 1
    } else {
      indexOfCount = Math.max(0, -indexOfCount - 1)
    }
    indexOfCount
  }

  case class DayAndRange(day: Int, start: Int, end: Int)

  def notificationCount(d: Int, expenditures: Seq[Int]) = {
    val exps = expenditures.toVector
    var counts = new java.util.ArrayList[Int](Collections.nCopies(201, 0))
    (0 until d)
      .map(exps(_))
      .foreach(exp => {
        val c = counts.get(exp)
        counts.set(exp, c + 1)
      })
    val sortedCounts = Array.fill(201)(0)
    var total = 0
    sortedCounts.indices
      .foreach(i => {
        val c = counts.get(i)
        val sortedCount = c + total
        sortedCounts(i) = sortedCount
        total = sortedCount
      })
    val middle = d / 2 + 1
    val medianPositions = if (d % 2 == 0) Seq(middle - 1, middle) else Seq(middle)

    var daysToMedians = Map[Int, Double]()
    (d until exps.size)
      .map(day => DayAndRange(day, day - d, day - 1))
      .foreach(dayAndRange => {
        val firstAmnt = exps(dayAndRange.start)
        val lastAmnt = exps(dayAndRange.end)
        if (dayAndRange.start > 0) {
          // add lastAmt to sortedCounts
          (lastAmnt until sortedCounts.length)
            .foreach(i => sortedCounts(i) = sortedCounts(i) + 1)
        }
        val medianValues = medianPositions.map(pos => indexByCount(sortedCounts, pos, sortedCounts.length - 1))
        val median = if (medianValues.size == 1) medianValues.head else medianValues.sum / 2.0
        daysToMedians = daysToMedians + (dayAndRange.day -> median)
        (firstAmnt until sortedCounts.length)
          .foreach(i => sortedCounts(i) = sortedCounts(i) - 1)
      })

    val notificationCount =
      (d until expenditures.size)
        .map(day => {
          val todaysExpenditure = expenditures(day)
          val medianExp = daysToMedians(day)
          if (todaysExpenditure >= 2.0 * medianExp)
            1
          else
            0
        }).sum
    notificationCount
  }
}

object Solution {

  def main(args: Array[String]): Unit = {
    val in = new Scanner(System.in)
    val solution = new Solution()

    val Seq(n, d) = readIntsFromLine(in)
      .take(2).toSeq
    val expenditures = readIntsFromLine(in)
      .take(n).toSeq

    val count = solution.notificationCount(d, expenditures)
    println(count)
  }

  def readLine(in: Scanner) = {
    in.nextLine()
  }

  def readIntFromLine(in: Scanner) = {
    readLine(in).toInt
  }

  def readIntsFromLine(in: Scanner) = {
    readLine(in).split(" ").map(s => s.toInt)
  }
}

