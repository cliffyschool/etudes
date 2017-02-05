import java.util.Scanner

class Solution {
    def permutationsStartingWithOne(zeroes: Int, ones: Int) = {
      if (ones == 0)
        0L
      else if (zeroes == 0)
        1L
      else {
        val n = zeroes + ones - 1
        val r = zeroes
        val nMinusR = n - r
        if (r >= nMinusR)
          (r + 1.toLong to n.toLong).product/ (1.toLong to nMinusR.toLong).product
        else {
          val top = ((nMinusR.toLong + 1) to n).product
          val bottom = (1.toLong to r.toLong).product
          top / bottom
        }
      }
    }
  def factorial(n :Int) : Long = {
    (1 to n).foldLeft(1)((x,y) => x * y)
  }
}

object Solution{

  def main(args: Array[String]) : Unit = {
    val in = new Scanner(System.in)
    val solution = new Solution()

    val testCaseCount = readIntFromLine(in)
    val results =
    (1 to testCaseCount)
      .map(i => {
       val args = readIntsFromLine(in)
        solution.permutationsStartingWithOne(args(0), args(1))
      })
    results.foreach(l => println(l))
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
