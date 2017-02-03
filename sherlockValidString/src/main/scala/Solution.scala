import java.util.Scanner

class Solution {
  def isValidInOneOrFewerRemoves(str: String) = {
    val distinctChars = str.groupBy(c => c).map(e => (e._1, e._2.length)).toMap
    val distinctCounts = distinctChars.values.toSet

    if (distinctCounts.size > 2)
      false
    else if (distinctCounts.size < 2)
      true
    else
    {
      val mostCommonCount = distinctCounts.max
      val lessCommonCount = distinctCounts.filter(c => c != mostCommonCount).head
      val countOccurrences = distinctChars.values
        .groupBy(cnt => cnt)
        .map(e => (e._1, e._2.size))
        .toMap
      val couldRemoveMostCommon = mostCommonCount - 1 == lessCommonCount &&
        countOccurrences(mostCommonCount) == 1
      val couldRemoveLeastCommon = lessCommonCount == 1 &&
        countOccurrences(lessCommonCount) == 1
      couldRemoveMostCommon || couldRemoveLeastCommon
    }
  }
}

object Solution{

  def main(args: Array[String]) : Unit = {
    val in = new Scanner(System.in)
    val solution = new Solution()

    val s = readLine(in)

    val result = solution.isValidInOneOrFewerRemoves(s)
    val toPrint = if (result) "YES" else "NO"


    System.out.println(toPrint)
  }

  def readLine(in: Scanner) = {
    in.nextLine()
  }

}