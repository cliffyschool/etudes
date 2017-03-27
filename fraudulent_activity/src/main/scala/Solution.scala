import java.util
import java.util.{Collections, Scanner}


class Solution {
  def indexByCount(sortedCounts: util.List[Int], i: Int) = {
    var indexOfCount = Collections.binarySearch(sortedCounts, i, CountOrdering)
    if (indexOfCount > 0) {
      while (indexOfCount - 1 >= 0 && i.equals(sortedCounts.get(indexOfCount - 1)))
        indexOfCount = indexOfCount - 1
    } else {
      indexOfCount = Math.max(0, -indexOfCount -1)
    }
    indexOfCount
  }
  object CountOrdering extends Ordering[Int] {
    override def compare(x: Int, y: Int): Int = Integer.compare(x, y)
  }


  case class Expenditure(amt: Int, originalPosition: Int)

  object ByAmountOrdering extends Ordering[Expenditure] {
    override def compare(x: Expenditure, y: Expenditure): Int = {
      val round1 = x.amt - y.amt
      if (round1 == 0)
        x.originalPosition - y.originalPosition
      else
        round1
    }
  }

  object ByPositionOrdering extends Ordering[Expenditure] {
    override def compare(x: Expenditure, y: Expenditure): Int = {
      x.originalPosition - y.originalPosition
    }
  }

  def findPos[T](li: util.LinkedList[T], x: T, ordering: Ordering[T])= {
    val index =Collections.binarySearch(li, x, ordering)
    if (index < 0)
      -index - 1
    else index
  }

  def insert[T](li: util.LinkedList[T], x: T)(implicit cmp: Ordering[T]) = {
    val insertAt = findPos(li, x, cmp)
    li.add(insertAt, x)
  }
  case class DayAndRange(day: Int, start:Int, end:Int)

  def notificationCount(d: Int, expenditures: Seq[Int]) = {
    val exps = expenditures.indices.map(i => Expenditure(expenditures(i), i)).toVector
    var counts = new java.util.ArrayList[Int](Collections.nCopies(200, 0))
    (0 until d)
      .map(exps(_))
      .foreach(exp => {
      val c = counts.get(exp.amt)
      counts.set(exp.amt, c + 1)
    })
    var sortedCounts = new util.ArrayList[Int](Collections.nCopies(200, 0))
    var total = 0
    (0 until d)
    .foreach(i => {
      val item = exps(i).amt
      val c = counts.get(item)
      val sortedCount = c + total
      sortedCounts.set(item, sortedCount)
      total = sortedCount
    })

   var daysToMedians = Map[Int,Double] ()
    (d until exps.size)
      .map(day => DayAndRange(day, day - d, day -1))
      .foreach(dayAndRange => {
        if (dayAndRange.start > 0) {
          val firstAmnt = exps(dayAndRange.start).amt
          val lastAmnt = exps(dayAndRange.end).amt
          // add lastAmt to sortedCounts
          sortedCounts.set(lastAmnt, sortedCounts.get(lastAmnt) + 1)
          if (firstAmnt < lastAmnt) {
            (firstAmnt until lastAmnt)
              .foreach(countIndex => sortedCounts.set(countIndex, sortedCounts.get(countIndex) - 1))
          } else if (firstAmnt > lastAmnt) {
            (lastAmnt until firstAmnt)
              .foreach(countIndex => sortedCounts.set(countIndex,sortedCounts.get(countIndex) + 1))
          }
        }
        val middle = dayAndRange.day / 2
        val medianPositions = if (d % 2 == 0) Seq(middle -1, middle) else Seq(middle)
        val medianValues = medianPositions.map(pos => indexByCount(sortedCounts, exps(pos).amt))
        val median = if( medianValues.size == 1) medianValues.head else medianValues.sum / 2.0
        daysToMedians = daysToMedians + (dayAndRange.day -> median)
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

