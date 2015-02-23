import org.specs2.mutable._

import scala.collection.Seq

class HierarchyFromRecords[T] {

  def printHierarchy(nodes: Traversable[Node[T]]) = {
    nodes.foreach(n => recursePrint(n, 0))
  }

  def recursePrint(node: Node[T], level: Int): Unit = {
    (0 to level).foreach(n => print("\t"))
    println(node.id)
    node.children.foreach(n => recursePrint(n, level+1))
  }

  def buildHierarchy(records: Seq[T],
                     groupByFunctions: List[(T => String)]) = {

    val flatNodes = records.map(r => generateFlatNodes(r, groupByFunctions)).flatten
    .groupBy(f => f.id).map(a => a._2(0)).toList

    val allParentIds = flatNodes.map(node => node.path).flatten
    val leaves = flatNodes.filter(node => !allParentIds.contains(node.id))
    .map(l => Node[T](l.id, List(), l.path))

    val toProcess = flatNodes.filterNot(f => leaves.exists(l => l.id == f.id ))
    val processed = process(toProcess,leaves)

    processed.filter(p => p.path.isEmpty).toList
  }

  def process(toProcess: List[FlatNode[T]], processed:List[Node[T]]) : List[Node[T]] = {
    if (toProcess.isEmpty)
      return processed

    val parent = toProcess.head
    if (!toProcess.map(_.path).flatten.contains(parent.id)){
      val children = processed.filter(c => c.path.lastOption == Some(parent.id)).toList
      val parentAsNode = Node[T](parent.id, children, parent.path)
      process(toProcess.tail, processed :+ parentAsNode)
    }
    else {
      process(toProcess.tail :+ toProcess.head, processed)
    }
  }

  def generateFlatNodes(record: T, groupByFunctions: List[(T => String)]) = {
    val nodeIds = groupByFunctions.scanLeft("")((a, b) => a + "/" + b.apply(record)).toList.tail

    val metaNodes = nodeIds.init.map(path => generateFlatNode(path, None)).toList
    val node = generateFlatNode(nodeIds.last, Some(record))

    metaNodes :+ node
  }

  def generateFlatNode(nodePath: String, maybeRecord: Option[T]) = {
    val fullPath = nodePath.split("/").tail
    val nodeId = fullPath.last
    val path = fullPath.init.toList
    FlatNode(nodeId, path, maybeRecord)
  }
}

case class Node[T](id:String, children:List[Node[T]], path:List[String])
case class FlatNode[T](id: String, path: List[String], record:Option[T])

class HierarchySpec extends Specification {

  case class Record(country: String, state: String, city: String, item: String)

  val defaultFunctions = List(
    (r: Record) => r.country,
    (r: Record) => r.state,
    (r: Record) => r.city,
    (r: Record) => r.item)

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

