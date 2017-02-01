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
      val outlierCountOccurences = distinctChars.values
        .groupBy(cnt => cnt)
        .map(e => (e._1, e._2.size))
        .filter(e => e._1 != mostCommonCount).values
        .headOption
      outlierCountOccurences.isEmpty || outlierCountOccurences.get == 1
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