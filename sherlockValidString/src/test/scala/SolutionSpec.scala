import java.util.Scanner
import scala.annotation.tailrec
import scala.collection.Seq
import scala.collection.mutable
import org.specs2.mutable._




class SolutionSpec extends Specification {
    val s = new Solution()
    var costs = Array(1, 2, 3, 4, 5, 6)
    var skippedItem = 0
    var charged = 10.0
    "Given fair charge" should {
        val result = s.owedToAnna(costs, skippedItem, charged)
        "return 0" in {
            result must equalTo(0.0)
         }
    }

    "Given overcharge" should {
        charged = 11
        val result = s.owedToAnna(costs, skippedItem, charged)
        "return 1" in {
            result must equalTo(1.0)
        }
    }

    "Given undercharge" should {
        charged = 8
        val result = s.owedToAnna(costs, skippedItem, charged)
        "return -2" in {
            result must equalTo(-2.0)
        }
    }

    "Given invalid index" should {
      skippedItem = costs.length
      charged = 0
      val halfTheBill = costs.sum / 2.0 * -1
      val result = s.owedToAnna(costs, skippedItem, 0)
      "return half the bill" in {
        result must equalTo(halfTheBill)
      }
    }
}
