import java.util.Scanner

import scala.util.Random

object Solution {

  def main(args: Array[String]): Unit = {
    val in = new Scanner(System.in)
    val solution = new Solution()
    val Array(numInts, diff) = getNAndK(in)

    val numbers = readNumbers(in, numInts)

    val count = solution.countPairsWithDifference(numbers, diff)

    println(count)
  }

  def getNAndK(in: Scanner) = {
    val nAndK = in.nextLine().split(" ")
    nAndK.map(_.toLong)
  }

  def readNumbers(in: Scanner, numLines: Long) = {
    in.nextLine().split(" ").map(s => s.toLong -> 0).toMap
  }
}

class Solution {
  // TODO
  def countPairsWithDifference(numbers: Map[Long,Int], diff: Long) = {
    val count = numbers
      .map(n => n._1 + diff)
      .count(n => numbers.contains(n))
    count
  }
}


import org.specs2.mutable._
class SolutionSpec extends Specification {
    "Given (3,2,1) and 1, countPairsWithDifference" should {
        val s = new Solution()
        val result = s.countPairsWithDifference(List(3L,2L,1L).map(_ -> 0).toMap, 1)
        "return 2" in {
            result must equalTo(2)
         }
    }

    "Given (1,5,3,4,2) and 2, countPairsWithDifference" should {
      val s = new Solution()
      val result = s.countPairsWithDifference(List(1L,5L,3L,4L,2L).map(_ -> 0).toMap, 2)
      "return 3" in {
        result must equalTo(3)
      }
    }

    "Given a massive list, and a large diff, countPairs" should {
      val s = new Solution()
      val rand = Random
      val list = (1 to 100000).map(i => rand.nextInt(9999999).toLong)
      val diff = list(0) - list(1)
      val result = s.countPairsWithDifference(list.map(_ -> 0).toMap, diff)
      "return a result" in {
        result must beGreaterThanOrEqualTo(1)
      }
    }
}
