package utils

import tree.ImprovedExpressionTree.{ImprovedExpressionTree, Node}

/**
 * Object to manage the print of the results.
 */
object PrintUtils {

  /**
   * Static method to print a tree in a pretty way.
   *
   * @param tree the tree to print.
   */
  def printTree(tree: ImprovedExpressionTree): Unit = {

    /**
     * Inner method to print a tree in a pretty way.
     *
     * @param tree the tree to print.
     * @param prefix the prefix to print before the current node.
     */
    def printLoop(tree: ImprovedExpressionTree, prefix: String): Unit = tree match {
        case n: Node => print(prefix); println(n.label())
          printLoop(n.l, prefix.replace("└──", "").replace("├──", "") + "    ├──")
          printLoop(n.r, prefix.replace("└──", "").replace("├──", "") + "    └──")
        case _ => print(prefix); print(tree.label()); println("")
    }

    print("└──")
    printLoop(tree, "")
  }

  /**
   * Static method to print the results of the evaluation of a solution in a pretty way.
   *
   * @param tree the tree to evaluate.
   * @param inputs the list of inputs.
   * @param outputs the list of outputs.
   */
  def printResults(tree: ImprovedExpressionTree, inputs: List[Double], outputs: List[Double]): Unit = {
    for (i <- inputs.indices) {
      println("Expected output for input " + inputs(i) + ": " + outputs(i))
      println("Result: " + tree.eval(inputs(i)) + "\n")
    }
  }
}

