import java.util.Scanner
import scala.annotation.tailrec
import scala.collection.Seq
import scala.collection.mutable
import org.specs2.mutable._

object Solution{

  def main(args: Array[String]) : Unit = {
    val in = new Scanner(System.in)
    val solution = new Solution()
    val numLines = getNumLines(in) 
    val lines = readLines(in, numLines) 

    // TODO: call solution
  }

  def getNumLines(in: Scanner) = {
    in.nextLine().toInt
  }

  def readLines(in: Scanner, numLines: Int) = {
    (1 to numLines).map(i => in.nextLine())
  }
}
class Solution {
    def rotateMatrix(matrix: Array[Array[Int]]) = {

      matrix
    }
}

class SolutionSpec extends Specification {
    "Given a matrix, rotateMatrix" should {
        val s = new Solution()
        val matrix = Array.ofDim[Int](5,4)
        matrix(0) = Array(1,2,3,4)
        matrix(1) = Array(7,8,9,10)
        matrix(2) = Array(13,14,15,16)
        matrix(3) = Array(19,20,21,22)
        matrix(4) = Array(25,26,27,28)

        val expected = Array.ofDim[Int](5,4)
        expected(0) = Array(28,27,26,25)
        expected(1) = Array(22,9,15,19)
        expected(2) = Array(16,8,21,13)
        expected(3) = Array(10,14,20,7)
        expected(4) = Array(4,3,2,1)
        val result = s.rotateMatrix(matrix)
        println(result)

        "rotate it" in {
            result(0) must equalTo(expected(0))
            result(1) must equalTo(expected(1))
            result(2) must equalTo(expected(2))
            result(3) must equalTo(expected(3))
            result(4) must equalTo(expected(4))
         }
    }
}
