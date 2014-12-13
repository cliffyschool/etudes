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

    // TODO: call solution
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
    def doStuff = ???
}

class SolutionSpec extends Specification {
    "Given x, solution" should {
        val s = new Solution()
        val result = s.doStuff()
        "return 2" in {
            result must equalTo(2)
         }
    }
}
