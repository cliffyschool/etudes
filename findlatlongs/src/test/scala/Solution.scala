import java.util.Scanner
import scala.annotation.tailrec
import scala.collection.Seq
import scala.collection.mutable
import org.specs2.mutable._

object Solution{

  def main(args: Array[String]) : Unit = {
    val in = new Scanner(System.in)
    val solution = new Solution()
    val numLines = getNumLines(in) 
    val lines = readLines(in, numLines) 
    lines.map(l => solution.latLong(l)).map(println)
  }

  def getNumLines(in: Scanner) = {
    in.nextLine().toInt
  }

  def readLines(in: Scanner, numLines: Int) = {
    (1 to numLines).map(i => in.nextLine())
  }
}
class Solution {
    def latLong(s: String) = {
        val regex = """\(([\-\+]?[0-9\.]+)\,\s+([\-\+]?[0-9\.]+)\)""".r
        regex.findAllMatchIn(s)
        match {
          case matches if !matches.hasNext => false
          case matches =>
            val m = matches.next 
            (m.group(1), m.group(2))
            match {
              case ll if validLatLong(ll._1.toFloat,ll._2.toFloat) => "Valid"
              case _ => "Invalid"
            }
        }
    }
    def validLatLong(lat: Float, long: Float) = lat >= -90.0 && lat <= 90.0 && long >= -180.0 && long <= 180.0
}

class SolutionSpec extends Specification {
    "Given parser, it" should {
        val s = new Solution()
        "parse (80, 80)" in {
            val result = s.latLong("(80, 80)")
            result must equalTo("Valid")
         }

        "parse negative and positive numbers" in {
            val result = s.latLong("(-80, +80)")
            result must equalTo("Valid")
        }
        "parse floats" in {
            val result = s.latLong("(77.11112223331, 149.99999999)")
            result must equalTo("Valid")
        }

        "parse extra space after comma" in {
            val result = s.latLong("(+80.5,  -120.354)")
            result must equalTo("Valid")
        }
        "disallow out-of-range lats" in {
            val result = s.latLong("(-91, 100)")
            result must equalTo("Invalid")
        }

        "disallow out-of-range longs" in {
            val result = s.latLong("(30, -190)")
            result must equalTo("Invalid")
        }
    }
}
