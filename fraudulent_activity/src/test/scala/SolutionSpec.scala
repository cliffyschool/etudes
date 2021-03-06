import org.scalatest._

import scala.io.Source

class SolutionSpec extends FlatSpec with Matchers {

  val solution = new Solution

  "Notifications" should "work for example" in {
    val d = 5
    val expenditures = Seq(2, 3, 4, 2, 3, 6, 8, 4, 5)
    val count = solution.notificationCount(d, expenditures)
    count should be(2)
  }

  "Notifications" should "work for even-numbered d" in {
    val d = 4
    val expenditures = Seq(4, 6, 8, 4, 6, 12, 16, 8, 10)
    val count = solution.notificationCount(d, expenditures)
    count should be(2)
  }

  "Notifications" should "be 0 for a short list" in {
    val d = 5
    val expenditures = Seq(2, 3, 4, 2, 3)
    val count = solution.notificationCount(d, expenditures)
    count should be(0)
  }
  "Notifications" should "work for a short d term" in {
    val d = 1
    val expenditures = Seq(2, 3, 4, 2, 3, 6, 8, 4, 5)
    val count = solution.notificationCount(d, expenditures)
    count should be(1)
  }
  "Notifications" should "work for large set" in {
    val d = 10000
    val expenditures = (0 to 200000)
      .map(i => if (i % 10000 == 0) 3 else 1)
    val count = solution.notificationCount(d, expenditures)
    count should be(20)
  }

  "find by count" should "find by index with multiple occurrences" in {
    val sortedCounts = Array(0, 2, 2, 3)
    val index = solution.indexByCount(sortedCounts, 1, sortedCounts.length - 1)
    index should be(1)
  }
  "find by count" should "return first index on multiple count matches" in {
    val sortedCounts = Array(0, 2, 2, 3)
    val index = solution.indexByCount(sortedCounts, 2, sortedCounts.length - 1)
    index should be(1)
  }
  "Notifications" should "work for paper example" in {
    val d = 3
    val expenditures = Seq(1, 4, 1, 2, 7, 5, 2)
    val count = solution.notificationCount(d, expenditures)
    count should be(3)
  }
  "Notifications" should "work from file" in {
    val d = 10000
    val n = 200000
    val exps = Source.fromFile("input01.txt").getLines.drop(1)
        .flatMap(_.split(" ").map(_.toInt))
      .toSeq
    val count = solution.notificationCount(d, exps)
    count should be(633)

  }
}
