import java.util.Scanner

import scala.annotation.tailrec
import scala.collection.Seq
import scala.collection.mutable

/**
 * Created by cfreeman on 12/12/14.
 */
object Solution{

  def main(args: Array[String]) : Unit = {
    val in = new Scanner(System.in)
    val solution = new Solution()
    val numLines = getNumLines(in) 
    val lines = readLines(in, numLines) 

    lines.map(_.toLong).map(l => solution.findDigits(l)).map(println)
  }

  def getNumLines(in: Scanner) = {
    in.nextLine().toInt
  }

  def readLines(in: Scanner, numLines: Int) = {
    (1 to numLines).map(i => in.nextLine())
  }
}
class Solution {
    def findDigits(num: Long) = {
        val digits = num.toString.toList.map(_.asDigit)
        val ret = digits.filterNot(d => d == 0).filter(d => num % d == 0).size
        if (num == 0) 1 else ret
    }
}
