import java.util
import java.util.{Collections, Scanner}


class Solution {

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

  def notificationCount(d: Int, expenditures: Seq[Int]) = {
    val exps = expenditures.indices.map(i => Expenditure(expenditures(i), i)).toVector
    var byAmountList = new util.LinkedList[Expenditure]()
    exps.take(d)
      .foreach(e => {
        byAmountList.add(e)
      })
    byAmountList.sort(ByAmountOrdering)
    val daysToMedians = (d until expenditures.size)
      .map(day => {
        val window = byAmountList
        val middle = window.size / 2
        val median = if (window.size % 2 == 0) {
          (window.get(middle - 1).amt + window.get(middle).amt) / 2.0
        } else
          window.get(middle).amt
        val toRemove = window.get(0)
        byAmountList.remove(0)
        if (day < exps.size) {
          val exp = exps(day)
          insert(byAmountList, exp)(ByAmountOrdering)
        }
        (day, median)
      }).toMap
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

