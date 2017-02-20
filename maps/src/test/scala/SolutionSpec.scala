import org.scalatest._
import Matchers._

class SolutionSpec extends FlatSpec with Matchers {

  val solution = Solution(Seq(("bob","12345678"), ("joe", "23456789"), ("jim", "11223344")))

  "map" should "contain known entry" in {
    val known = solution.get("joe")
    known shouldEqual("joe=23456789")
  }

  it should "return None for bogus key" in {
    val result = solution.get("josephine")
    result shouldEqual("Not found")
  }
}
