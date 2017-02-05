import org.specs2.mutable._


class SolutionSpec extends Specification {
    val s = new Solution()
    "Given 2, 3" should {
        val result = s.permutationsStartingWithOne(2, 3)
        "return 6" in {
            result must equalTo(6)
         }
    }
    "Given 1, 5" should {
        val result = s.permutationsStartingWithOne(1, 5)
        "return 5" in {
            result must equalTo(5)
        }
    }
    "Given 2, 4" should {
        val result = s.permutationsStartingWithOne(2, 4)
        "return 10" in {
            result must equalTo(10)
        }
    }
    "Given 0 0's" should {
        val result = s.permutationsStartingWithOne(0, 5)
        "return 1" in {
            result must equalTo(1)
        }
    }
    "Given 0 1's" should {
        val result = s.permutationsStartingWithOne(5, 0)
        "return 0" in {
            result must equalTo(0)
        }
    }
    "Given a large number of 0's" should {
        val result = s.permutationsStartingWithOne(400, 4)
        "return something" in {
            result must beGreaterThan(1L)
        }
    }
    "Given a large number of 1's" should {
        val result = s.permutationsStartingWithOne(4, 300)
        "return something" in {
            result must beGreaterThan(1L)
        }
    }
}
