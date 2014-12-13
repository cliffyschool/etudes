import org.specs2.mutable._

/**
 * Created by cfreeman on 12/12/14.
 */
class IsFiboSpec extends Specification {

  "fibosUntil(5)" should {
    val isFib = new IsFibo()
    val fibs = isFib.fibosUntil(5)
    "return 0,1,1,2,3,5" in {
      fibs must equalTo(Seq(0,1,1,2,3,5))
    }
  }

  "fibosUntil(10)" should {
    val isFib = new IsFibo()
    val fibs = isFib.fibosUntil(10)
    "return 0,1,1,2,3,5,8,13" in {
      fibs must equalTo(Seq(0, 1, 1, 2, 3, 5, 8, 13))
    }
  }

  "fibosUntil(0)" should {
    val isFibo = new IsFibo()
    val fibs = isFibo.fibosUntil(0)

    "return 0" in {
      fibs must equalTo(Seq(0))
    }
  }

  "fibosUntil(1)" should {
    val isFibo = new IsFibo()
    val fibs = isFibo.fibosUntil(1)

    "return 0,1" in {
      fibs must equalTo(Seq(0, 1))
    }
  }
}

