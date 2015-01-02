import java.util.Scanner
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.{mutable, Seq}
import org.specs2.mutable._

class TreeBuilder {
    def buildTree(nodes: List[Node]) : Seq[TreeNode] = {
      val nodesById = mutable.Map(nodes.map(n => (n.id, n)).toMap.toSeq: _*)
      val parentIdsToChildIds = new mutable.HashMap[Int, mutable.Set[Int]] with mutable.MultiMap[Int,Int]
      nodes.filter(n => n.parentId.isDefined).map(n => parentIdsToChildIds.addBinding(n.parentId.get, n.id))

      val newTree = mutable.Map[Int,TreeNode]()
      val leaves = nodesById.filterNot(entry => parentIdsToChildIds.contains(entry._1)).map(_._2)
      leaves.map(leaf => moveToNewTree(TreeNode(leaf.id, mutable.Set()),  nodesById, newTree))

      while (!nodesById.isEmpty){
        val nodesToMove =
        nodesById
          .flatMap{entry =>
            parentIdsToChildIds.get(entry._1) match {
              case Some(ids) => Some((entry._1, ids))
              case None => None
            }
         }.toSeq
        .filter(e => e._2.subsetOf(newTree.keys.toSet))
        .map(e => TreeNode(e._1, e._2.map(id => newTree(id))))

        nodesToMove.map(node => moveToNewTree(node, nodesById, newTree))
      }

      val allChildren = parentIdsToChildIds.values.flatten.map(id => id -> id).toMap
      val rootNodes = newTree.values.filterNot(n => allChildren.contains(n.id)).toSeq
      rootNodes
    }

    private def moveToNewTree(node: TreeNode, oldTree: mutable.Map[Int,Node], newTree: mutable.Map[Int,TreeNode]) = {
      newTree.put(node.id, node)
      oldTree.remove(node.id)
    }
}

case class Node(id: Int, parentId: Option[Int])
case class TreeNode(id: Int, children: mutable.Set[TreeNode])

class SolutionSpec extends Specification {
    "Given a parent and child, the tree" should {
        val s = new TreeBuilder()
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
    val s = new TreeBuilder()
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
