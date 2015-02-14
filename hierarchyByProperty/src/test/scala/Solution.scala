import java.util.Scanner
import scala.annotation.tailrec
import scala.collection.Seq
import scala.collection.mutable
import org.specs2.mutable._
import scala.collection.immutable

object Solution{

  def main(args: Array[String]) : Unit = {
    val in = new Scanner(System.in)
    val solution = new Solution()
    val numLines = getNumLines(in) 
    val lines = readLines(in, numLines) 

    // TODO: call solution
  }

  def getNumLines(in: Scanner) = {
    in.nextLine().toInt
  }

  def readLines(in: Scanner, numLines: Int) = {
    (1 to numLines).map(i => in.nextLine())
  }
}
class Solution {
    def buildTree(records: Seq[Record]) = {
      val functions: List[(Record => String)] = List(
        (r:Record) => r.country,
        (r:Record) => r.state,
        (r:Record) => r.city,
        (r:Record) => r.item)

      val root = Node("", records.toList, List())
      val newRoot = recurseBuild(root, functions, 0)
      newRoot.children
    }

    def recurseBuild( parent: Node,
                      getValFunctions: List[Function1[Record,String]], 
                      functionIndex: Int ) : Node = {
      if (getValFunctions.size <= functionIndex)
        return parent;

      val childNodes = parent.records.groupBy(getValFunctions(functionIndex))
                          .map(m => Node(m._1, m._2.toList, List()))

      childNodes.map(n => println(n.id))

      val rChildNodes = childNodes.map(c => recurseBuild(c, getValFunctions, functionIndex+1)).toList
      val newParent = Node(parent.id, parent.records, rChildNodes)

      return newParent
    }
}


case class Record(country: String, state: String, city: String, item: String)
case class Node(id: String, records: List[Record], children: List[Node])

class SolutionSpec extends Specification {
    "Given records, buildTree" should {
        val s = new Solution()
        val records = List(
          Record("USA", "Texas", "Denton", "123"),
          Record("USA", "Oklahoma", "Oklahoma City", "345"),
          Record("Canada", "British Columbia", "Vancouver", "766"),
          Record("Canada", "Ontario", "Toronto", "3643"),
          Record("USA", "Texas", "Dallas", "7346")
          )
        val result = s.buildTree(records)
        "return 'USA' and 'Canada' in top list" in {
          result.size must equalTo(2)
          result(0).id must equalTo("Canada")
          result(1).id must equalTo("USA")
         }
         "return 'Texas' as child of 'USA'" in {
           result(1).children(1).id must equalTo("Texas")
         }
    }
}
