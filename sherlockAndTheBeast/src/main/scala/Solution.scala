import java.util.Scanner
import scala.annotation.tailrec
import scala.collection.Seq
import scala.collection.mutable
import org.specs2.mutable._

object Solution{

  def main(args: Array[String]) : Unit = {
    val in = new Scanner(System.in)
    val solution = new Solution()
    val numLines = getNumLines(in) 
    val lines = readLines(in, numLines) 

    lines.map(line => solution.calculateLargestDecentNumber(line.toInt))
    .map(println)
  }

  def getNumLines(in: Scanner) = {
    in.nextLine().toInt
  }

  def readLines(in: Scanner, numLines: Int) = {
    (1 to numLines).map(i => in.nextLine())
  }
}
class Solution {
    // TODO
    def calculateLargestDecentNumber(digits: Int) = {
      val multiplesOf3 = (0 to (digits / 3).floor.toInt).map(d => d*3)
      val operandPairs = multiplesOf3.map(three => (three,digits-three))
      val matchingPairs = operandPairs.filter(p => p._2 % 5 == 0)
        .filterNot(p => p._1 == 0 && p._2 == 0)
        .sortBy(_._2)
     
      val digitList = matchingPairs
        .headOption
        .map(p => (List.fill(p._1)('5') ++ List.fill(p._2)('3')).mkString)
      digitList.getOrElse("-1")
    }
}
