import java.util
import java.util.Scanner

import scala.collection.{SortedSet, mutable}

class Solution {
  case class Expenditure(amt: Int, originalPosition: Int)
  trait HasExpenditure {
    def exp : Expenditure
  }
  case class ExpenditureByAmount(exp: Expenditure) extends Comparable[ExpenditureByAmount] with HasExpenditure {
    override def compareTo(that: ExpenditureByAmount): Int = {
      val round1 = exp.amt - that.exp.amt
      if (round1 == 0)
        exp.originalPosition - that.exp.originalPosition
      else
        round1
    }
  }
  case class ExpenditureByOriginalPos(exp: Expenditure) extends Comparable[ExpenditureByOriginalPos] with HasExpenditure {
    override def compareTo(that: ExpenditureByOriginalPos): Int = {
      exp.originalPosition - that.exp.originalPosition
    }
  }


  def notificationCount(d: Int, expenditures: Seq[Int]) = {
    val exps = expenditures.indices.map(i => Expenditure(expenditures(i), i))
    val byAmount = new java.util.TreeSet[ExpenditureByAmount]()
    exps.foreach(e => byAmount.add(ExpenditureByAmount(e)))
    val byPos = new java.util.TreeSet[ExpenditureByOriginalPos]()
    exps.foreach(e => byPos.add(ExpenditureByOriginalPos(e)))
    1
  }

    /*
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
  */

  def medianElements(range: Seq[(Int,Int)]) = {
    val middleIndex = range.size / 2
    if (range.size % 2 == 0) {
      range.slice(middleIndex - 1, middleIndex)
    } else {
      Seq(range(middleIndex))
    }
  }

  def notificationsOnDay(recent: Seq[(Int,Int)], todaysExpenditure: Int) = {
    val medianExp: Float = medianExpenditures(recent)
    if (todaysExpenditure >= 2 * medianExp)
      1
    else
      0
  }


  def medianExpenditures(range: Seq[(Int,Int)]): Float = {
    val middle = range.size / 2
    if (range.size % 2 == 0) {
      (range(middle)._1 + range(middle + 1)._1) / 2
    }
    else
      range(middle)._1
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

