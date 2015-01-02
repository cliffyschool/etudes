import org.specs2.mutable.Specification

import scala.annotation.tailrec
import scala.collection.{mutable, Seq}

/**
 * This implementation gets rid of the while loop and is recursive.
 */
class SecondTreeBuilder {
  def buildTree(nodes: List[Node]) : Seq[TreeNode] = {
    val nodesById = nodes.map(n => (n.id, n)).toMap
    val parentIdsToChildIds = nodes.filter(n => n.parentId.isDefined)
    .foldLeft(new mutable.HashMap[Int,mutable.Set[Int]] with mutable.MultiMap[Int,Int])((mapSoFar, node) =>
      mapSoFar.addBinding(node.parentId.get, node.id))

    val leaves = nodesById.filterNot(entry => parentIdsToChildIds.contains(entry._1)).map(_._2)
    val initialTree = addNodes(Map.empty[Int,TreeNode], leaves.map(n => TreeNode(n.id, mutable.Set())))
    val nodeMapWithoutLeaves = removeNodes(nodesById, leaves.map(_.id))

    val tree = makeTree(nodeMapWithoutLeaves, parentIdsToChildIds, initialTree)

    val allChildren = parentIdsToChildIds.values.flatten.map(id => id -> id).toMap
    val rootNodes = tree.values.filterNot(n => allChildren.contains(n.id)).toSeq
    rootNodes
  }

  private def addNodes(nodeMap: Map[Int,TreeNode], nodes:Iterable[TreeNode]) =
    nodes.foldLeft(nodeMap)((mapSoFar,node) => mapSoFar + (node.id -> node))

  private def removeNodes(nodeMap: Map[Int,Node], nodeIds: Iterable[Int]) =
    nodeIds.foldLeft(nodeMap)((mapSoFar, nodeId) => mapSoFar - nodeId)


  @tailrec
  private def makeTree(nodesById: Map[Int,Node], parentIdsToChildrenIds: mutable.MultiMap[Int,Int],
                        tree: Map[Int,TreeNode])
  : Map[Int,TreeNode] = {

    if (nodesById.isEmpty)
      return tree

    val nodesToMove =
      nodesById
        .flatMap{entry =>
        parentIdsToChildrenIds.get(entry._1) match {
          case Some(ids) => Some((entry._1, ids))
          case None => None
        }
      }.toSeq
        .filter(e => e._2.subsetOf(tree.keys.toSet))
        .map(e => TreeNode(e._1, e._2.map(id => tree(id))))

    val newTree = addNodes(tree, nodesToMove)
    val newNodeMap = removeNodes(nodesById, nodesToMove.map(_.id))

    makeTree(newNodeMap, parentIdsToChildrenIds, newTree)
  }

}

  class SecondTreeBuilderSpec extends Specification {
    "Given a parent and child, the tree" should {
      val s = new SecondTreeBuilder()
      val nodes = List(Node(1, None), Node(2, Some(1)))
      val result = s.buildTree(nodes)
      "be a list of size 1" in {
        result must haveSize(1)
      }
      "have the parent node" in {
        result(0).id must equalTo(1)
      }
      "have the child node in the parent node's child list" in {
        result(0).children.map(c => c.id) must contain(2)
      }
    }
    "Given a parent and child and grandchild, the tree" should {
      val s = new SecondTreeBuilder()
      val nodes = List(Node(1, None), Node(2, Some(1)), Node(3, Some(2)))
      val result = s.buildTree(nodes)
      "be a list of size 1" in {
        result must haveSize(1)
      }
      "have the parent node" in {
        result(0).id must equalTo(1)
      }
      "have the child node in the parent node's child list" in {
        result(0).children.map(c => c.id) must contain(2)
      }
      "have the grandchild node in the child's child list" in {
        result(0).children.toSeq(0).children.toSeq(0).id must equalTo(3)
      }
    }

  }
