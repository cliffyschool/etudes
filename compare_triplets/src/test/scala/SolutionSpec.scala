import collection.mutable.Stack
import org.scalatest._

class SolutionSpec extends FlatSpec with Matchers {

  val solution = new Solution

  "Score" should "give a point for a higher score in each category" in {
    val score = solution.score(Seq(1,2,3), Seq(0,1,2))
    score._1 should be (3)
  }

  it should "give 0 points for a lower score in each category" in {
    val score = solution.score(Seq(1,2,3), Seq(0,1,2))
    score._2 should be (0)
  }
}
