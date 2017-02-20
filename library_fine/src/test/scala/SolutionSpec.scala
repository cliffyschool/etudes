import collection.mutable.Stack
import org.scalatest._

class SolutionSpec extends FlatSpec with Matchers {

  val solution = new Solution

  "Fine" should "account for years late book" in {
    val fine = solution.calculateFine(Seq(1,2,2003), Seq(1,2,2005))
    fine should be (10000)
  }

  "Fine" should "account for months late book in same year" in {
    val dueMonth = 2
    val monthsLate = 3
    val fine = solution.calculateFine(Seq(1,dueMonth,2003), Seq(1,dueMonth+monthsLate,2003))
    fine should be (monthsLate * 500)
  }
  "Fine" should "account for days late book in same months" in {
    val dueDay = 2
    val daysLate = 19
    val fine = solution.calculateFine(Seq(dueDay,3,2003), Seq(dueDay+daysLate,3,2003))
    fine should be (daysLate * 15)
  }
  "Fine" should "account for on time book" in {
    val due = Seq(1,3,2004)
    val fine = solution.calculateFine(due, due)
    fine should be (0)
  }
  "Fine" should "account for early book" in {
    val due = Seq(1,3,2004)
    val early = Seq(27,2,2004)
    val fine = solution.calculateFine(due, early)
    fine should be (0)
  }
}
