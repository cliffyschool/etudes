import java.util.Scanner

class Solution {

    def notificationCount(d: Int, expenditures: Seq[Int]) = {
      val notificationCount = expenditures.indices
        .map(notificationsOnDay(expenditures, _, d)).sum
      notificationCount
    }

    def notificationsOnDay(expenditures: Seq[Int], index: Int, d: Int) = {
      if (index < d)
        0
      else {
        val recent = expenditures.slice(index - d, index)
        val medianExp: Float = medianExpenditures(recent)
        if (expenditures(index) >= 2 * medianExp)
          1
        else
          0
      }
    }


    def medianExpenditures(slice: Seq[Int]) : Float = {
      val sorted = slice//.sorted
      val middle = slice.size / 2
      if (slice.size % 2 == 0)
        (sorted(middle) + sorted(middle + 1)) / 2
      else 
        sorted(middle)
    }
}

object Solution{

  def main(args: Array[String]) : Unit = {
    val in = new Scanner(System.in)
    val solution = new Solution()

    val Seq(n,d) = readIntsFromLine(in)
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
