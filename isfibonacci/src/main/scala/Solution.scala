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
    val numLines = in.nextLine().toInt
    val nums = (1 to numLines).map(i => in.nextLine().toInt)
    nums.map(i => fibo.isFibo(i)).map {
      case false => "IsNotFibo"
      case true => "IsFibo"
    }.map(println)
  }
}
class Solution {

  def isFibo(i: Int) = {
      val fibos = fibosUntil(i)
      if (fibos.isEmpty) false else fibos.last == i
  }

  def fibosUntil(i: Int) = {
    i match {
      case n if n < 0 => Seq()
      case 0 => Seq(0)
      case n => generateFibosUntil(i, Seq(0,1))
    }
  }

  @tailrec
  private def generateFibosUntil(maxFibo: Int, fibos: Seq[Int]) : Seq[Int]= {
    fibos.last match {
      case last if last >= maxFibo => fibos
      case n => generateFibosUntil(maxFibo, fibos ++ Seq(fibos.takeRight(2).sum))
    }
  }
}
