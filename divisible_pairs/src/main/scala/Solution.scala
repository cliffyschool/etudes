import java.util.Scanner

class Solution {
  // TODO
  def divisiblePairs(nums: Seq[Int], k: Int) = {
    val divisiblePairs =
      nums.indices
        .flatMap(i => (i + 1 until nums.size).map(j => (nums(i), nums(j))))
        .map(pair => pair._1 + pair._2)
        .count(sum => sum % k == 0)
    divisiblePairs
  }
}

object Solution {

  def main(args: Array[String]): Unit = {
    val in = new Scanner(System.in)
    val solution = new Solution()

    val nAndK = readIntsFromLine(in)
    val nums = readIntsFromLine(in).take(nAndK.head)
    val divPairs = solution.divisiblePairs(nums.toSeq, nAndK.last)

    System.out.println(divPairs)
  }

  def readLine(in: Scanner) = {
    in.nextLine()
  }

  def readIntFromLine(in: Scanner) = {
    readLine(in).toInt
  }

  def readIntsFromLine(in: Scanner) = {
    readLine(in).split(" ").map(s => s.toInt)
  }
}
