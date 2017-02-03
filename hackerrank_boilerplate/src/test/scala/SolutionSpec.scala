import java.util.Scanner
import scala.annotation.tailrec
import scala.collection.Seq
import scala.collection.mutable
import org.specs2.mutable._


class SolutionSpec extends Specification {
    val s = new Solution()
    "Given something" should {
        val result = s.doSomething
        "return 0" in {
            result must equalTo(0.0)
         }
    }
}
