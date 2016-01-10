import java.util.Scanner
import scala.collection.Seq
import scala.collection.mutable

object Solution{

  def main(args: Array[String]) : Unit = {
    val in = new Scanner(System.in)
    val solution = new Solution()
    val configLine = readLines(in, 1)(0)

    val rowsColsRotations = getInts(configLine).toList
    val matrixStrings = readLines(in, rowsColsRotations(0))
    val matrix = matrixStrings.map(s => getInts(s).toArray).toArray
    val sol = new Solution()
    val rotated = sol.rotateMatrix(matrix, rowsColsRotations(2))
    rotated.groupBy(_.row).toSeq.sortBy(_._1).map(_._2.map(_.num)).map(v => {v.map(n => print(s"$n ")); println})
  }

  def getNumLines(in: Scanner) = {
    in.nextLine().toInt
  }

  def readLines(in: Scanner, numLines: Int) = {
    (1 to numLines).map(i => in.nextLine())
  }

  def getInts(line: String ) = {
    line.split(" ").map(_.toInt)
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
      val (row, col) = 
      (cell.row, cell.col) match {
       case (topLeft.row, topLeft.col) => (topLeft.row + 1, topLeft.col)
       case (bottomRight.row, bottomRight.col) => (bottomRight.row -1, bottomRight.col)
       //topRight
       case (topLeft.row, bottomRight.col) => (topLeft.row, bottomRight.col - 1)
       // bottomLeft
       case (bottomRight.row, topLeft.col) => (bottomRight.row, topLeft.col + 1)
       // top
       case (topLeft.row, col: Int) => (topLeft.row, col - 1)
       // left
       case (row: Int, topLeft.col) => (row + 1, topLeft.col)
       // bottom
       case (bottomRight.row, col: Int) => (bottomRight.row, col + 1)
       // right
       case (row: Int, bottomRight.col) => (row - 1, bottomRight.col)
     }
    Cell(row, col, cell.num)
    }
}
