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
    
    lines.map(_.toLong).map(n => solution.treeHeight(n)).map(println)
  }

  def getNumLines(in: Scanner) = {
    in.nextLine().toInt
  }

  def readLines(in: Scanner, numLines: Int) = {
    (1 to numLines).map(i => in.nextLine())
  }
}
class Solution {

    def treeHeight(n: Long) = {
        (1.toLong to n).fold[Long](1){ (accum:Long, curr:Long) =>
            if (curr % 2 == 0) accum + 1
            else accum * 2
        }
    }
}

class SolutionSpec extends Specification {
    "Given 0, solution" should {
        val s = new Solution()
        val result = s.treeHeight(0)
        "return 1" in {
            result must equalTo(1)
         }
    }


    "Given 2, solution" should {
        val s = new Solution()
        val result = s.treeHeight(2)
        "return 3" in {
            result must equalTo(3)
         }
    }

    "Given 3, solution" should {
        val s = new Solution()
        val result = s.treeHeight(3)
        "return 6" in {
            result must equalTo(6)
         }
    }


    "Given 4, solution" should {
        val s = new Solution()
        val result = s.treeHeight(4)
        "return 7" in {
            result must equalTo(7)
         }
    }
}
