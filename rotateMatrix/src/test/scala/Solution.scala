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

case class Cell(row: Int, col: Int, num: Int)

class Solution {

    def rotateMatrix(matrix: Array[Array[Int]], times: Int) = {
      val maxRow = matrix.length - 1
      val maxCol = matrix(0).length - 1
      val levelCount = Math.min(matrix.length, matrix(0).length) / 2
      val corners = (0 until levelCount)
        .map(l => l -> (Cell(l,l, matrix(l)(l)), Cell(maxRow-l, maxCol-l, matrix(maxRow-l)(maxCol-l)))).toMap
      println(corners)
      val rotated = (0 to maxRow)
        .flatMap(row => (0 to maxCol)
          .map(col => Cell(row,col,matrix(row)(col))))
        .map(cell => {
          val cellCorners = corners(cellLevel(cell, 0, maxRow, 0, maxCol))
          rotateCell(cell, cellCorners._1, cellCorners._2, times)})
            .sortBy(cell => (cell.row,cell.col))

      rotated
    }

    def cellLevel(cell: Cell, minRow: Int, maxRow: Int, minCol: Int, maxCol: Int) = {
      Seq(cell.row-minRow, maxRow-cell.row, cell.col-minCol, maxCol-cell.col).min
    }

    def rotateCell(cell: Cell, topLeft:Cell, bottomRight: Cell, times: Int) = {
      (0 until times)
        .foldLeft(cell)( (cell,index) => nextPos(cell,topLeft,bottomRight))
    }

    def nextPos(cell: Cell, topLeft: Cell, bottomRight: Cell) = {
     cell match {
       case c @ Cell(topLeft.col, topLeft.row, topLeft.num) =>
         Cell(topLeft.row + 1, topLeft.col, topLeft.num)
       case c @ Cell(bottomRight.row, bottomRight.col, bottomRight.num) =>
         Cell(bottomRight.row -1, bottomRight.col, bottomRight.num)
       //topRight
       case c @ Cell(topLeft.row, bottomRight.col, _) =>
         Cell(topLeft.row, bottomRight.col - 1, c.num)
       // bottomLeft
       case c @ Cell(bottomRight.row, topLeft.col, _) =>
         Cell(bottomRight.row, topLeft.col + 1, c.num)
       // top
       case c @ Cell(topLeft.row,_, _) =>
         Cell(c.row, c.col - 1, c.num)
       // left
       case c @ Cell(_, topLeft.col, _) =>
         Cell(c.row + 1, c.col, c.num)
       // bottom
       case c @ Cell(bottomRight.row, _, _) =>
         Cell(c.row, c.col + 1, c.num)
       // right
       case c @ Cell(_, bottomRight.col, _) =>
         Cell(c.row - 1, c.col, c.num)
     }
    }
}

class SolutionSpec extends Specification {

    "Given topLeft, nextPos" should {
      val sol = new Solution();
      val topLeft = Cell(0,0,1)
      val bottomRight = Cell(1,1,2)
      val rotated = sol.nextPos(topLeft, topLeft, bottomRight)
      "rotate it once" in {
        rotated must equalTo(Cell(1,0,1))
        
      }
    }
/*
    "Given a small matrix, rotateMatrix" should {
      val sol = new Solution();
      val matrix = Array.ofDim[Int](2,2)
      matrix(0) = Array(1,2)
      matrix(1) = Array(3,4)
     
      val rotated = sol.rotateMatrix(matrix,1)
      println(rotated)
      "rotate it" in {
        rotated must contain(
          Cell(0,0,2),
          Cell(0,1,4),
          Cell(1,0,1),
          Cell(1,1,3))
      }
    }
    */
    
    "Given a matrix, rotateMatrix" should {
        val s = new Solution()
        val matrix = Array.ofDim[Int](5,4)
        matrix(0) = Array(1,2,3,4)
        matrix(1) = Array(7,8,9,10)
        matrix(2) = Array(13,14,15,16)
        matrix(3) = Array(19,20,21,22)
        matrix(4) = Array(25,26,27,28)

      val expected = Array.ofDim[Int](5,4)
        matrix(0) = Array(2,3,4,10)
        matrix(1) = Array(1,9,15,16)
        matrix(2) = Array(7,8,21,22)
        matrix(3) = Array(13,14,20,28)
        matrix(4) = Array(19, 25,26,27)

        val expectedAfter7 = Array.ofDim[Int](5,4)
        expected(0) = Array(28,27,26,25)
        expected(1) = Array(22,9,15,19)
        expected(2) = Array(16,8,21,13)
        expected(3) = Array(10,14,20,7)
        expected(4) = Array(4,3,2,1)
        
        val result = s.rotateMatrix(matrix, 1)
println(result)
        "rotate it" in {
            result must contain(
              Cell(0,0,2),Cell(1,1,9),Cell(2,2,21),Cell(3,3,28)
            )
         }
    }
}
