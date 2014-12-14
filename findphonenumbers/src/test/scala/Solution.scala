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
    lines.map(l => solution.phoneNumber(l)).map(println)
  }

  def getNumLines(in: Scanner) = {
    in.nextLine().toInt
  }

  def readLines(in: Scanner, numLines: Int) = {
    (1 to numLines).map(i => in.nextLine())
  }
}
class Solution {
    def phoneNumber(s: String) = {
        val regex = """(\d{1,3})[\- ](\d{1,3})[\- ](\d{4,10})""".r
        val m = regex.findAllMatchIn(s).next()
        s"CountryCode=${m.group(1)},LocalAreaCode=${m.group(2)},Number=${m.group(3)}"
    }
}

class SolutionSpec extends Specification {
    "Given parser, it" should {
        val s = new Solution()
        "parse 123-456-7890" in {
            val result = s.phoneNumber("123-456-7890")
            result must equalTo("CountryCode=123LocalAreaCode=456Number=7890")
         }

        "parse 123 456 7890" in {
            val result = s.phoneNumber("123 456 7890")
            result must equalTo("CountryCode=123LocalAreaCode=456Number=7890")
         }

        "parse 12 34 5678910282" in {
            val result = s.phoneNumber("12 34 5678910282")
            result must equalTo("CountryCode=12LocalAreaCode=34Number=5678910282")
         }
    }
}
