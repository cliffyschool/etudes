import collection.mutable.Stack
import org.scalatest._

class SolutionSpec extends FlatSpec with Matchers {

  val solution = new Solution

  "Notifications" should "work for example" in {
    val d = 5
    val expenditures = Seq(2,3,4,2,3,6,8,4,5)
    val count = solution.notificationCount(d, expenditures)
    count should be (2)
  }
  
  "Notifications" should "be 0 for a short list" in {
    val d = 5
    val expenditures = Seq(2,3,4,2,3)
    val count = solution.notificationCount(d, expenditures)
    count should be (0)
  }
  "Notifications" should "work for a short d term" in {
    val d = 1
    val expenditures = Seq(2,3,4,2,3, 6, 8, 4, 5)
    val count = solution.notificationCount(d, expenditures)
    count should be (1)
  }
}
