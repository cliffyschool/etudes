import java.util.Scanner
import org.specs2.mutable._

object Solution{

  def main(args: Array[String]) : Unit = {
    val in = new Scanner(System.in)
    val solution = new Solution()
    val line = in.nextLine()
    val isPangram = solution.isPangram(line)
    println(isPangram)

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
  def isPangram(sentence: String) = {
    val allLetters = 'a' to 'z'
    allLetters.map(c => sentence.toLowerCase().contains(c)).forall(b => b) match {
      case false => "not pangram"
      case true => "pangram"
    }
  }
}

class SolutionSpec extends Specification {
    "Given string with all letters of the alphabet, isPangram" should {
        val s = new Solution()
        val allLetters = "abcdefghijklmnopqrstuvwxyz"
        val result = s.isPangram(allLetters)
        "return pangram" in {
            result must equalTo("pangram")
         }
    }

  "Given string with all but one letter of the alphabet, isPangram" should {
    val s = new Solution()
    val allLetters = "abcdefghijklmnopqrstuvwxyz"
    val result = s.isPangram(allLetters.dropRight(1))
    "return not pangram" in {
      result must equalTo("not pangram")
    }
  }

  "Given string with all letters and one letter in caps, isPangram" should {
    val s = new Solution()
    val allLetters = "Abcdefghijklmnopqrstuvwxyz"
    val result = s.isPangram(allLetters)
    "return pangram" in {
      result must equalTo("pangram")
    }
  }
}
