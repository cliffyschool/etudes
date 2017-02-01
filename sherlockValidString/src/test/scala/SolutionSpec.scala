import org.specs2.mutable._




class SolutionSpec extends Specification {
    val s = new Solution()
    var str = "aabbcc"
    "Given string with equal letter counts" should {
        val result = s.isValidInOneOrFewerRemoves(str)
        "return true" in {
            result must equalTo(true)
         }
    }
  "Given string with only one outlier" should {
    str = "aabbccdde"
    val result = s.isValidInOneOrFewerRemoves(str)
    "return true" in {
      result must equalTo(true)
    }
  }
  "Given string with two outliers" should {
    str = "aabbccddef"
    val result = s.isValidInOneOrFewerRemoves(str)
    "return false" in {
      result must equalTo(false)
    }
  }
}
