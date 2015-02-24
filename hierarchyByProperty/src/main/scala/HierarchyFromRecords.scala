import org.scalatest._

import scala.collection.Seq

class HierarchyFromRecords[T] {

  case class Node[T](id:String, children:List[Node[T]], path:List[String])
  case class FlatNode[T](id: String, path: List[String], record:Option[T])

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
                    .groupBy(f => f.id).map(a => a._2.head).toList

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
