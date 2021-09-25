package tree

import org.scalatest.funsuite.AnyFunSuite
import tree.ImprovedExpressionTree.{AddNode, ImprovedExpressionTree, InputNode, Leaf, MulNode, SubNode}

class TreeTest extends AnyFunSuite {

  private val leaf: Leaf = InputNode()
  private val tree: ImprovedExpressionTree = AddNode(leaf, leaf)

  test(s"A tree with a single node should have depth = 0"){
    assert(leaf.depth() == 0)
  }

  test(s"A tree with three nodes should have depth = 1"){
    assert(tree.depth() == 1)
  }

  test(s"The node at depth = 0 is the root of the tree"){
    assert(tree.getNodesAtDepth(0) == List(tree))
  }

  test(s"The nodes at depth = 1 are the two nodes connected with the root"){
    assert(tree.getNodesAtDepth(1) == List(leaf, leaf))
  }

  test(s"A tree composed by an AddNode and two inputs should compute the sum of the inputs, multiplied by the multiplied factors"){
    assert(AddNode(InputNode(1), InputNode(1), 1).eval(1) == 2)
  }

  test(s"A tree composed by a SubNode and two inputs should compute the subtraction of the inputs, multiplied by the multiplied factors"){
    assert(SubNode(InputNode(1), InputNode(1), 1).eval(1) == 0)
  }

  test(s"A tree composed by a MulNode and two inputs should compute the multiplication of the inputs, multiplied by the multiplied factors"){
    assert(MulNode(InputNode(1), InputNode(1), 0.5).eval(1) == 0.5)
  }

}
