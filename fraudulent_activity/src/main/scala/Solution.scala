import java.util.Scanner

import scala.collection.mutable

class Solution {

  def notificationCount(d: Int, expenditures: Seq[Int]) = {
    /*
    var window = mutable.PriorityQueue[(Int,Int)]()(Ordering.by[(Int,Int), Int](e => e._1).reverse)
    (0 until d).map(i => (expenditures(i), i)).foreach(window += _)
    */
    //var window = (0 until d).map(i => (expenditures(i), i)).sortBy(x => x)
    val sorted = expenditures.indices.map(x => (expenditures(x), x)).sortBy(_._1)
    val dayAndSearchIndices = (d until sorted.size)
      .map(sorted(_)._2)
      .map(day => {
        val middle = day - Math.ceil(d / 2.0).toInt
        val medianIndicesSearchEnd =
          if (d % 2 == 0 && d > 1) {
            Seq(middle, middle - 1)
          } else Seq(middle)
        (day, medianIndicesSearchEnd)
      })
    val dayAndMedians = dayAndSearchIndices.map(dayAndSearchEnd => {
      val medians =
        dayAndSearchEnd._2.flatMap(searchEnd => (searchEnd to Math.max(dayAndSearchEnd._1 - d, 0) by -1)
          .find(idx => {
            val originalPos = sorted(idx)._2
            originalPos >= dayAndSearchEnd._1 - d && originalPos < dayAndSearchEnd._1
          }))
        .map(x => sorted(x)._1)
      (dayAndSearchEnd._1, medians)
    })
    val dayAndMedian = dayAndMedians
      .map(dayAndMedians =>
        if(dayAndMedians._2.size == 1)
        (dayAndMedians._1, dayAndMedians._2.head.toDouble)
        else
          (dayAndMedians._1, dayAndMedians._2.sum.toDouble / 2))
  val notificationCount = dayAndMedian
      .map(dAndM => {
        val todaysExpenditure = expenditures(dAndM._1)
        if (todaysExpenditure >= dAndM._2.*(2.0)) 1 else 0
      }).sum

    notificationCount
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

