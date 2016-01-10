import java.util.Scanner
import scala.annotation.tailrec
import scala.collection.Seq
import scala.collection.mutable
import org.specs2.mutable._

class SolutionSpec extends Specification {

    "Given topLeft, nextPos" should {
      val sol = new Solution()
      val topLeft = Cell(0,0,1)
      val bottomRight = Cell(1,1,2)
      val rotated = sol.nextPos(topLeft, topLeft, bottomRight)
      "rotate it once" in {
        rotated must equalTo(Cell(1,0,1))
        
      }
    }

    "Given a small matrix, rotateMatrix" should {
      val sol = new Solution();
      val matrix = Array.ofDim[Int](2,2)
      matrix(0) = Array(1,2)
      matrix(1) = Array(3,4)
     
      val rotated = sol.rotateMatrix(matrix,1)
      "rotate it" in {
        rotated must contain(
          Cell(0,0,2),
          Cell(0,1,4),
          Cell(1,0,1),
          Cell(1,1,3))
      }
    }

    "Given a matrix, rotateMatrix" should {
        val s = new Solution()
        val matrix = Array.ofDim[Int](5,4)
        matrix(0) = Array(1,2,3,4)
        matrix(1) = Array(7,8,9,10)
        matrix(2) = Array(13,14,15,16)
        matrix(3) = Array(19,20,21,22)
        matrix(4) = Array(25,26,27,28)

      val expected = Array.ofDim[Int](5,4)
        expected(0) = Array(2,3,4,10)
        expected(1) = Array(1,9,15,16)
        expected(2) = Array(7,8,21,22)
        expected(3) = Array(13,14,20,28)
        expected(4) = Array(19, 25,26,27)

      val expectedCells = (0 until expected.length)
      .flatMap(row => (0 until expected(0).length)
      .map(col => Cell(row, col, expected(row)(col))))

        val result = s.rotateMatrix(matrix, 1)
        "rotate it once" in {
          println(result)
            result must containTheSameElementsAs(expectedCells)
         }
    }

    "Given a matrix, rotateMatrix" should {
      val s = new Solution()
      val matrix = Array.ofDim[Int](5,4)
      matrix(0) = Array(1,2,3,4)
      matrix(1) = Array(7,8,9,10)
      matrix(2) = Array(13,14,15,16)
      matrix(3) = Array(19,20,21,22)
      matrix(4) = Array(25,26,27,28)
      val expectedAfter7 = Array.ofDim[Int](5,4)
      expectedAfter7(0) = Array(28,27,26,25)
      expectedAfter7(1) = Array(22,9,15,19)
      expectedAfter7(2) = Array(16,8,21,13)
      expectedAfter7(3) = Array(10,14,20,7)
      expectedAfter7(4) = Array(4,3,2,1)

      val expectedCells = (0 until expectedAfter7.length)
      .flatMap(row => (0 until expectedAfter7(0).length)
      .map(col => Cell(row, col, expectedAfter7(row)(col))))

      val result = s.rotateMatrix(matrix, 7)
      "rotate it 7 times" in {
        result must containTheSameElementsAs(expectedCells)
      }
    }
}
