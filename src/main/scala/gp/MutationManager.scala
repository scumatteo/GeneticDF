package gp

import tree.ImprovedExpressionTree.{GaussianNode, ImprovedExpressionTree, Node}
import tree.TreeManager
import utils.DoubleUtils

import scala.util.Random

/**
 * Enumeration that represents the possible mutation types of a GaussianNode.
 * A GaussianNode can mutate entirely, by changing its multiplier factor, its mean and its variance, or it can mutate
 * only a single parameter at a time.
 */
object GaussianMutationType extends Enumeration {
  type GaussianMutationType = Value
  val ENTIRE_MUTATION, MP_MUTATION, MEAN_MUTATION, VARIANCE_MUTATION = Value
}

/**
 * Object to manage the mutations.
 */
object MutationManager {

  /**
   * Static method to perform mutations.
   * A random depth is chosen, then a random node at this depth is chosen for the mutation.
   *
   * @param tree the tree in which a mutation should be applied.
   *
   * @return the new IE-Tree with the mutation.
   */
  def performMutation(tree: ImprovedExpressionTree): ImprovedExpressionTree = {
    val randomDepth = Random.nextInt(tree.depth() + 1)

    val treeLevel = tree.getNodesAtDepth(randomDepth)

    val randomSubtree: ImprovedExpressionTree = treeLevel(Random.nextInt(treeLevel.size))

    randomSubtree match {
      case g: GaussianNode =>

        val mutationType = GaussianMutationType(Random.nextInt(GaussianMutationType.maxId))

        mutationType match {
          case GaussianMutationType.ENTIRE_MUTATION =>
            val newNode = GaussianNode(DoubleUtils.getRandomDouble(), DoubleUtils.getRandomDouble(), DoubleUtils.getRandomDouble())
            mutate(tree, randomSubtree, newNode)
          case GaussianMutationType.MP_MUTATION =>
            val newNode = GaussianNode(g.mean, g.variance, DoubleUtils.getRandomDouble())
            mutate(tree, randomSubtree, newNode)
          case GaussianMutationType.MEAN_MUTATION =>
            val newNode = GaussianNode(DoubleUtils.getRandomDouble(), g.variance, g.multiplierFactor)
            mutate(tree, randomSubtree, newNode)
          case GaussianMutationType.VARIANCE_MUTATION =>
            val newNode = GaussianNode(g.mean, DoubleUtils.getRandomDouble(), g.multiplierFactor)
            mutate(tree, randomSubtree, newNode)
        }


      case t =>
        val depth = tree.depth() - t.depth()
        val newNode = TreeManager.createRandomTree(depth)
        mutate(tree, randomSubtree, newNode)
    }
  }

  /**
   * Private method to perform the mutation at the selected node.
   *
   * @param oldTree the initial tree.
   * @param oldNode the initial node.
   * @param newNode the new node to substitute to the old node.
   * @return the new IE-Tree.
   */
  private def mutate(oldTree: ImprovedExpressionTree, oldNode: ImprovedExpressionTree, newNode: ImprovedExpressionTree): ImprovedExpressionTree = oldTree match {
    case n: Node if n.l == oldNode => n.replaceChildren(newNode, n.r.clone())
    case n: Node if n.r == oldNode => n.replaceChildren(n.l.clone(), newNode)
    case n: Node => n.replaceChildren(mutate(n.l, oldNode, newNode), mutate(n.r, oldNode, newNode))
    case _ => oldTree
  }

}
