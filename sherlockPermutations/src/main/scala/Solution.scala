import java.util.Scanner

class Solution {
  val modBy = (Math.pow(10, 9) + 7).toLong

    def permutationsStartingWithOne(zeroes: Int, ones: Int) = {
      if (ones == 0)
        0L
      else if (zeroes == 0)
        1L
      else {
        val n = zeroes + ones - 1
        val r = zeroes
        val nMinusR = n - r
        /*
        val top = ((nMinusR.toLong + 1) to n).product
        val bottom = (1.toLong to r.toLong).product
        top / bottom
        */
        val topNums = (nMinusR + 1 to n)
        val bottomNums = if (r < topNums.size) Seq(1) else Seq() ++ (1 to r)
        assert(topNums.size == bottomNums.size)
        val total =
          (0 to topNums.size - 1)
            .map(i => (topNums(i), bottomNums(i)))
            .foldLeft(1L)((total, topAndBottom) => Math.floorDiv(total * topAndBottom._1, topAndBottom._2).toLong % modBy)
        total.toLong
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
