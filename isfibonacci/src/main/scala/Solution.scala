import java.util.Scanner

import scala.annotation.tailrec
import scala.collection.Seq
import scala.collection.mutable

/**
 * Created by cfreeman on 12/12/14.
 */
object Solution{

  def main(args: Array[String]) : Unit = {
    val in = new Scanner(System.in)
    val fibo = new Solution()
    val numLines = getNumLines(in) 
    val lines = readLines(in, numLines) 
    lines.map(l => l.toLong).map(i => fibo.isFibo(i)).map(println)
  }

  def getNumLines(in: Scanner) = {
    in.nextLine().toInt
  }

  def readLines(in: Scanner, numLines: Int) = {
    (1 to numLines).map(i => in.nextLine())
  }
}
class Solution {

  def isFibo(i: Long) = {
      fibosUntil(i) match {
        case f if !f.isEmpty && f.last == i => "IsFibo"
        case f => "IsNotFibo"
      }
  }

  def fibosUntil(i: Long) = {
    i match {
      case n if n < 0 => Seq()
      case 0 => Seq(0)
      case n => generateFibosUntil(i, Seq(0,1))
    }
  }

  @tailrec
  private def generateFibosUntil(maxFibo: Long, fibos: Seq[Long]) : Seq[Long]= {
    fibos.last match {
      case last if last >= maxFibo => fibos
      case n => generateFibosUntil(maxFibo, fibos ++ Seq(fibos.takeRight(2).sum))
    }
  }
}
