import scala.annotation.tailrec
import scala.collection.Seq

/**
 * Created by cfreeman on 12/12/14.
 */
class IsFibo {
  def fibosUntil(i: Int) = {
    i match {
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
