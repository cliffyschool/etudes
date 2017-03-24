import java.util.Scanner


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

  def findPos[T](li: scala.collection.IndexedSeq[T], x: T, ordering: Ordering[T])= {
    scala.collection.Searching.search(li)
      .search(x, 0, li.size)(ordering)
      .insertionPoint
  }

  def insert[T](li: scala.collection.IndexedSeq[T], x: T)(implicit cmp: Ordering[T]) = {
    val insertAt = findPos(li, x, cmp)
    li.patch(insertAt, IndexedSeq(x), 0)
  }

  def notificationCount(d: Int, expenditures: Seq[Int]) = {
    val exps = expenditures.indices.map(i => Expenditure(expenditures(i), i)).toVector
    var byAmountList = exps.take(d).sorted(ByAmountOrdering).to[scala.collection.IndexedSeq]
    var byPositionList = exps.take(d).sorted(ByPositionOrdering).to[scala.collection.IndexedSeq]
    val daysToMedians = (d until expenditures.size)
      .map(day => {
        val window = byAmountList
        val middle = window.size / 2
        val median = if (window.size % 2 == 0) {
          window.slice(middle - 1, middle + 1).map(_.amt).sum / 2
        } else
          window(middle).amt
        val toRemove = window.head
        byAmountList = byAmountList.tail
        val posToRemove = findPos(byPositionList, toRemove, ByPositionOrdering)
        byPositionList = byPositionList.patch(posToRemove, Nil, 1)
        if (day < exps.size) {
          val exp = exps(day)
          byAmountList = insert(byAmountList, exp)(ByAmountOrdering)
          byPositionList = insert(byPositionList, exp)(ByPositionOrdering)
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

