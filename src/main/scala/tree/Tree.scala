package tree

import tree.FunctionSet.FunctionSet
import tree.TerminalSet.TerminalSet
import utils.{DoubleUtils, GaussianUtils}

/**
 * Set of function F.
 */
object FunctionSet extends Enumeration {
  type FunctionSet = Value
  val ADD, SUB, MUL = Value
}

/**
 * Set of terminal T.
 */
object TerminalSet extends Enumeration {
  type TerminalSet = Value
  val CONST, INPUT, GAUSSIAN = Value
}

/**
 * Companion object for the IE-Tree.
 */
object ImprovedExpressionTree {


  /**
   * Constructor for the function nodes.
   * @param function the type of function.
   * @param left the left child.
   * @param right the right child.
   * @param multiplierFactor the multiplier factor part (mp).
   *
   * @return the new function node.
   */
  def apply(function: FunctionSet, left: ImprovedExpressionTree, right: ImprovedExpressionTree, multiplierFactor: Double = DoubleUtils.getRandomDouble()): Node = function match {
    case FunctionSet.ADD => AddNode(left, right, multiplierFactor)
    case FunctionSet.MUL => MulNode(left, right, multiplierFactor)
    case FunctionSet.SUB => SubNode(left, right, multiplierFactor)
  }

  /**
   * Constructor for ConstNode.
   * @param terminal the type of node.
   * @param value the constant value of the node.
   * @param multiplierFactor the multiplier factor part (mp).
   *
   * @return the new ConstNode.
   */
  def apply(terminal: TerminalSet, value: Double, multiplierFactor: Double): Leaf = terminal match {
    case TerminalSet.CONST => ConstNode(value, multiplierFactor)
  }

  /**
   * Constructor for InputNode.
   * @param terminal the type of node.
   * @param multiplierFactor the multiplier factor part (mp).
   *
   * @return the new InputNode.
   */
  def apply(terminal: TerminalSet, multiplierFactor: Double): Leaf = terminal match {
    case TerminalSet.INPUT => InputNode(multiplierFactor)

  }

  /**
   * Constructor for GaussianNode
   * @param terminal the type of node.
   * @param mean the mean of the gaussian.
   * @param variance the variance of gaussian.
   * @param multiplierFactor the multiplier factor part (mp).
   *
   * @return the new GaussianNode
   */
  def apply(terminal: TerminalSet, mean: Double, variance: Double, multiplierFactor: Double): Leaf = terminal match {
    case TerminalSet.GAUSSIAN => GaussianNode(mean, variance, multiplierFactor)
  }

  /**
   * Trait that represents an IE-Tree. It is extends as its structure part (sp) and it has a multiplier factor (mp).
   */
  sealed trait ImprovedExpressionTree {

    /**
     * Multiplier factor part (mp).
     */
    val multiplierFactor: Double

    /**
     * Method that evaluate the IE-Tree.
     * @param input the input value to evaluate the IE-Tree.
     *
     * @return the value computed by this IE-Tree.
     */
    def eval(input: Double): Double = this.eval(input)

    /**
     * Method to estimate the depth of the IE-Tree, with this as root.
     *
     * @return the depth of this IE-Tree.
     */
    def depth(): Int = this match {
      case n: Node => math.max(n.l.depth(), n.r.depth()) + 1
      case _ => 0
    }

    /**
     * Method that returns the label that represents a node.
     *
     * @return the label of the node.
     */
    def label(): String

    /**
     * Method that makes a deep copy of the IE-Tree.
     *
     * @return the cloned tree.
     */
    override def clone(): ImprovedExpressionTree = this.clone()

    /**
     * Method to get all the nodes at a given depth.
     *
     * @param depth the given depth.
     * @return a sequence of nodes at the given depth.
     */
    def getNodesAtDepth(depth: Int): Seq[ImprovedExpressionTree] = {

      /**
       * Inner method that loops over the tree.
       *
       * @param d the depth.
       * @param tree the tree.
       * @return the sequence of nodes at the given depth.
       */
      def getNodes(d: Int, tree: ImprovedExpressionTree): List[ImprovedExpressionTree] = tree match {
        case _ if d == 0 => List(tree)
        case n: Node =>
          getNodes(d - 1, n.l) ::: getNodes(d - 1, n.r)
        case _ => Nil

      }
      getNodes(depth, this)
    }

    /**
     * Method to copy an IE-Tree modifying its multiplier factor part (mp).
     * @param multiplierFactor the new multiplier factor.
     *
     * @return a copy of the node with the new multiplier factor.
     */
    def copyWithMP(multiplierFactor: Double): ImprovedExpressionTree
  }

  /**
   * Node is a particular case of IE-Tree. It has two children: l (for the left) and r (for the right).
   * It represents the type of the Function F (that are binary operator) of the problem.
   */
  sealed trait Node extends ImprovedExpressionTree {
    val l: ImprovedExpressionTree
    val r: ImprovedExpressionTree

    /**
     * Method to create a copy of the node, replacing its children.
     *
     * @param left the left child.
     * @param right the right child.
     *
     * @return the new node.
     */
    def replaceChildren(left: ImprovedExpressionTree, right: ImprovedExpressionTree): Node
  }

  /**
   * Leaf is a particular case of IE-Tree that has no children.
   * It represents the type of the Terminal T of the problem.
   */
  sealed trait Leaf extends ImprovedExpressionTree

  /**
   * Node that implements the addition operation between two sub-trees.
   *
   * @param l the left child.
   * @param r the right child.
   * @param multiplierFactor the multiplier factor part (mp) of the node.
   */
  case class AddNode(override val l: ImprovedExpressionTree,
                     override val r: ImprovedExpressionTree,
                     override val multiplierFactor: Double = DoubleUtils.getRandomDouble()) extends Node {

    override def label(): String = " + "

    override def eval(input: Double): Double = this.multiplierFactor * (l.eval(input) + r.eval(input))

    override def clone(): AddNode = AddNode(this.l.clone(), this.r.clone(), multiplierFactor)

    override def replaceChildren(left: ImprovedExpressionTree, right: ImprovedExpressionTree): Node = AddNode(left, right, multiplierFactor)

    override def copyWithMP(multiplierFactor: Double): ImprovedExpressionTree = AddNode(this.l, this.r, multiplierFactor)
  }

  /**
   * Node that implements the subtraction operation between two sub-trees.
   *
   * @param l the left child.
   * @param r the right child.
   * @param multiplierFactor the multiplier factor part (mp) of the node.
   */
  case class SubNode(override val l: ImprovedExpressionTree,
                     override val r: ImprovedExpressionTree,
                     override val multiplierFactor: Double = DoubleUtils.getRandomDouble()) extends Node {

    override def label(): String = " - "

    override def eval(input: Double): Double = this.multiplierFactor * (l.eval(input) - r.eval(input))

    override def clone(): SubNode = SubNode(this.l.clone(), this.r.clone(), multiplierFactor)

    override def replaceChildren(left: ImprovedExpressionTree, right: ImprovedExpressionTree): Node = SubNode(left, right, multiplierFactor)

    override def copyWithMP(multiplierFactor: Double): ImprovedExpressionTree = SubNode(this.l, this.r, multiplierFactor)


  }

  /**
   * Node that implements the multiplication operation between two sub-trees.
   *
   * @param l the left child.
   * @param r the right child.
   * @param multiplierFactor the multiplier factor part (mp) of the node.
   */
  case class MulNode(override val l: ImprovedExpressionTree,
                     override val r: ImprovedExpressionTree,
                     override val multiplierFactor: Double = DoubleUtils.getRandomDouble()) extends Node {

    override def label(): String = " * "

    override def eval(input: Double): Double = this.multiplierFactor * (l.eval(input) * r.eval(input))

    override def clone(): MulNode = MulNode(this.l.clone(), this.r.clone(), multiplierFactor)

    override def replaceChildren(left: ImprovedExpressionTree, right: ImprovedExpressionTree): Node = MulNode(left, right, multiplierFactor)

    override def copyWithMP(multiplierFactor: Double): ImprovedExpressionTree = MulNode(this.l, this.r, multiplierFactor)

  }

  /**
   * Leaf that implements a constant value.
   *
   * @param value the constant value of the node.
   * @param multiplierFactor the multiplier factor part (mp) of the node.
   */
  case class ConstNode(value: Double, override val multiplierFactor: Double = DoubleUtils.getRandomDouble()) extends Leaf {

    override def label(): String = this.value.toString //.take(3)

    override def eval(input: Double): Double = this.multiplierFactor * this.value

    override def clone(): ConstNode = ConstNode(this.value, multiplierFactor)

    override def copyWithMP(multiplierFactor: Double): ImprovedExpressionTree = ConstNode(this.value, multiplierFactor)

  }

  /**
   * Leaf that represents an input value.
   *
   * @param multiplierFactor the multiplier factor part (mp) of the node.
   */
  case class InputNode(override val multiplierFactor: Double = DoubleUtils.getRandomDouble()) extends Leaf {

    override def label(): String = "X"  //.take(3)

    override def eval(input: Double): Double = this.multiplierFactor * input

    override def clone(): InputNode = InputNode(multiplierFactor)

    override def copyWithMP(multiplierFactor: Double): ImprovedExpressionTree = InputNode(multiplierFactor)

  }

  /**
   * Leaf that implements a gaussian function.
   *
   * @param mean the mean of the gaussian.
   * @param variance the variance of the gaussian.
   * @param multiplierFactor the multiplier factor part (mp) of the node.
   */
  case class GaussianNode(mean: Double, variance: Double, override val multiplierFactor: Double = DoubleUtils.getRandomDouble()) extends Leaf {

    override def label(): String = "N(X | " + mean.toString /*.take(3) */ + ", " + variance.toString /*.take(3) */ + ")"

    override def eval(input: Double): Double = this.multiplierFactor * GaussianUtils.getGaussianValue(input, mean, variance)

    override def clone(): GaussianNode = GaussianNode(this.mean, this.variance, multiplierFactor)

    override def copyWithMP(multiplierFactor: Double): ImprovedExpressionTree = GaussianNode(this.mean, this.variance, multiplierFactor)

  }

}
