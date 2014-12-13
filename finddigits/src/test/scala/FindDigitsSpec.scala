import org.specs2.mutable._

/**
 * Created by cfreeman on 12/12/14.
 */
class FindDigitsSpec extends Specification {
    "Given 125, findDigits" should {
        val s = new Solution()
        val count = s.findDigits(125)
        "return 2" in {
            count must equalTo(2)
         }
    }

    "Given 24, findDigits" should {
        val s = new Solution()
        val count = s.findDigits(24)
        "return 2" in {
            count must equalTo(2)
        }
    }

    "Given 100, findDigits" should {
        val s = new Solution()
        val count = s.findDigits(100)
        "return 1" in {
            count must equalTo(1)
        }
    }


    "Given 0, findDigits" should {
        val s = new Solution()
        val count = s.findDigits(0)
        "return 1" in {
            count must equalTo(1)
        }
    }
}
