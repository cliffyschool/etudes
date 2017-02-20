import java.util.Scanner

class Solution {
    def score(first: Seq[Int], second: Seq[Int]) = {
      first.indices
        .map(i =>{ 
          if (first(i) < second(i))
            (0, 1)
          else if (first(i) > second(i))
            (1,0)
          else
            (0,0)
        })
      .foldLeft((0,0))((t,a) => (t._1 + a._1, t._2 + a._2))
    }
}

object Solution{

  def main(args: Array[String]) : Unit = {
    val in = new Scanner(System.in)
    val solution = new Solution()

    val firstLine = readIntsFromLine(in)
      .take(3).toSeq
    val secondLine = readIntsFromLine(in)
      .take(3).toSeq

    val score = solution.score(firstLine, secondLine)

    println(score._1 + " " + score._2)
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
