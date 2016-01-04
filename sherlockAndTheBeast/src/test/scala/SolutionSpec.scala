
import java.util.Scanner
import scala.annotation.tailrec
import scala.collection.Seq
import scala.collection.mutable
import org.specs2.mutable._

class SolutionSpec extends Specification {
    "Given a random int N, the decent number" should {
        val s = new Solution()
        val n = new java.util.Random().nextInt(12345)
        val result = s.calculateLargestDecentNumber(n)
        println(result)
        "be N digits long" in {
            result.length must equalTo(n)
         }
         "have a number of 3's that is divisible by 5" in {
           result.filter(_ == '3').size % 5 must equalTo(0)
         }
         "have a number of 5's that is divisible by 3" in {
           result.filter(_ == '5').size % 3 must equalTo(0)
         }
        "have a final digit equal to or lesser than the first digit" in {
            result.charAt(result.length-1).toInt must beLessThanOrEqualTo(result.charAt(0).toInt)
        }
    }

    "Given a 0, the decent number" should {
      val s = new Solution()
      val result = s.calculateLargestDecentNumber(0)
      "be -1" in {
        result must equalTo("-1")
      }
    }
}
