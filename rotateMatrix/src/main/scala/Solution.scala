import java.util.Scanner
import scala.annotation.tailrec
import scala.collection.Seq
import scala.collection.mutable

object Solution{

  def main(args: Array[String]) : Unit = {
    val in = new Scanner(System.in)
    val solution = new Solution()
    val configLine = readLines(in, 1)(0)

    val rowsColsRotations = configLine.split(' ').map(_.toInt).toList
    val matrixStrings = readLines(in, rowsColsRotations(0))
    val matrix = matrixStrings.map(s => s.split(' ').map(_.toInt).toArray).toArray
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
     cell match {
       case c @ Cell(topLeft.row, topLeft.col, _) =>
         Cell(topLeft.row + 1, topLeft.col, c.num)
       case c @ Cell(bottomRight.row, bottomRight.col,_) =>
         Cell(bottomRight.row -1, bottomRight.col, c.num)
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
