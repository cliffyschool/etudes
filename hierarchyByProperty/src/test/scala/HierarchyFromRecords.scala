import org.specs2.mutable._

import scala.collection.Seq

class HierarchyFromRecords[T] {

    def buildHierarchy(   records: Seq[T],
                          groupByFunctions: List[(T => String)]) = {

      val root = Node[T]("", records.toList, List())
      val newRoot = recurseBuild(root, groupByFunctions, 0)
      newRoot.children
    }

    def printHierarchy(nodes: Traversable[Node[T]]) = {
      nodes.foreach(n => recursePrint(n, 0))
    }

    def recursePrint(node: Node[T], level: Int): Unit = {
      (0 to level).foreach(n => print("\t"))
      println(node.id)
      node.children.foreach(n => recursePrint(n, level+1))
    }

    def recurseBuild( parent: Node[T],
                      getValFunctions: List[(T) => String],
                      functionIndex: Int ) : Node[T] = {
      if (getValFunctions.size <= functionIndex)
        return parent

      val childNodes = parent.records.groupBy(getValFunctions(functionIndex))
                          .map(m => Node(m._1, m._2.toList, List()))

      val rChildNodes = childNodes.map(c => recurseBuild(c, getValFunctions, functionIndex+1)).toList
      val newParent = Node(parent.id, parent.records, rChildNodes)

      newParent
    }
}

case class Node[T](id: String, records: List[T], children: List[Node[T]])

class HierarchyFromPropertiesSpec extends Specification {

  case class Record(country: String, state: String, city: String, item: String)
  val defaultFunctions = List(
    (r:Record) => r.country,
    (r:Record) => r.state,
    (r:Record) => r.city,
    (r:Record) => r.item)

    "Given records, buildTree" should {
        val s = new HierarchyFromRecords[Record]()
        val records = List(
          Record("USA", "Texas", "Denton", "123"),
          Record("USA", "Oklahoma", "Oklahoma City", "345"),
          Record("Canada", "British Columbia", "Vancouver", "766"),
          Record("Canada", "Ontario", "Toronto", "3643"),
          Record("USA", "Texas", "Dallas", "7346")
          )

        val result = s.buildHierarchy(records, defaultFunctions)
        "have 2 items in the list" in {
          result.size must equalTo(2)
        }
        "contain USA" in {
          val ids = result.map(r => r.id).toSeq
          ids must contain("USA")
        }
        "contain Canada" in {
          val ids = result.map(r => r.id).toSeq
          ids must contain("Canada")
        }
         "return Texas under USA" in {
           val usa = result.find(n => n.id == "USA").get
           usa.children.map(n => n.id) must contain("Texas")
         }
        "return Denton under Texas" in {
          val texas = result.find(n => n.id == "USA").get
          .children.find(n => n.id == "Texas").get
          texas.children.map(n => n.id) must contain("Denton")
        }
        "return 123 under Denton" in {
          val denton = result.find(n => n.id == "USA").get
          .children.find(n => n.id == "Texas").get
          .children.find(n => n.id == "Denton").get
          denton.children.map(n => n.id) must contain("123")
        }
    }
  "Given print" should {
    val s = new HierarchyFromRecords[Record]()
    val records = List(
      Record("USA", "Texas", "Denton", "123"),
      Record("USA", "Oklahoma", "Oklahoma City", "345"),
      Record("Canada", "British Columbia", "Vancouver", "766"),
      Record("Canada", "Ontario", "Toronto", "3643"),
      Record("USA", "Texas", "Dallas", "7346")
    )
    val byCountry = List((r:Record) => r.country, (r:Record) => r.item)
    val result = s.buildHierarchy(records, byCountry)
    "do it" in {
      s.printHierarchy(result)
      0 should equalTo(0)
    }
  }
}
