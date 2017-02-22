import java.util.Scanner
import scala.annotation.tailrec
import scala.collection.Seq
import scala.collection.mutable
import org.specs2.mutable._


class SolutionSpec extends Specification {
    val s = new Solution()
    "Given (1,3,2,6,1,2), 3" should {
        val result = s.divisiblePairs(Seq(1,3,2,6,1,2), 3)
        "return 5" in {
            result must equalTo(5)
         }
    }
}
