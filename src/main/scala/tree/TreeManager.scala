package tree

import gp.GPManager
import tree.ImprovedExpressionTree._
import utils.DoubleUtils

import scala.annotation.tailrec
import scala.util.Random

/**
 * Object that implements methods to manage IE-Trees.
 */
object TreeManager {

  /**
   * Static method to generate an initial population.
   *
   * @param populationSize the initial population size.
   *
   * @return a list of IE-Tree that is the initial population.
   */
  def createInitialPopulation(populationSize: Int): List[ImprovedExpressionTree] = {

    /**
     * Inner method that loops over the number of the initial population.
     *
     * @param nTrees the initial population size.
     * @param population an accumulator for the initial population.
     *
     * @return the initial population.
     */
    @tailrec
    def create(nTrees: Int, population: List[ImprovedExpressionTree]) : List[ImprovedExpressionTree] = nTrees match {
      case 0 => population
      case _ => create(nTrees - 1, TreeManager.createRandomTree() :: population)
    }

    create(populationSize, Nil)
  }

  /**
   * Static method to create a random tree.
   *
   * @param depth the initial depth.
   *
   * @return an IE-Tree randomly created.
   */
  def createRandomTree(depth: Int = 0): ImprovedExpressionTree = depth match {
    case d if d >= GPManager.MAX_TREE_DEPTH =>
      createTerminal()
    case _ =>
      val random = Random.nextInt(FunctionSet.maxId + TerminalSet.maxId)
      if (random > FunctionSet.maxId) {
        this.createTerminal()
      }
      else {
        ImprovedExpressionTree(FunctionSet.values.toSeq(random % TerminalSet.maxId),
          createRandomTree(depth + 1), createRandomTree(depth + 1))
      }
  }

  /**
   * Private method to create a terminal.
   * If the node is a terminal T, then with a probability of gaussianProbability a GaussianNode is generated.
   *
   * @return the terminal node created.
   */
  private def createTerminal(): ImprovedExpressionTree = {
    val gaussianProbability = Random.nextDouble()
    if (gaussianProbability < GPManager.GAUSSIAN_PROBABILITY) {
      ImprovedExpressionTree(TerminalSet.GAUSSIAN, DoubleUtils.getRandomDouble(), DoubleUtils.getRandomDouble(), multiplierFactor = DoubleUtils.getRandomDouble())
    }
    else {
      val terminal = TerminalSet.values.toSeq(Random.nextInt(TerminalSet.maxId - 1))
      terminal match {
        case TerminalSet.CONST => ImprovedExpressionTree(terminal, DoubleUtils.getRandomDouble(), multiplierFactor = DoubleUtils.getRandomDouble())
        case _ => ImprovedExpressionTree(terminal, multiplierFactor = DoubleUtils.getRandomDouble())
      }
    }
  }

  /*

  /**
   * Static method to generate an initial population.
   * @param populationSize the initial population size.
   * @param maxDepth the max depth of the trees.
   * @param gaussianProbability the probability of create a GaussianNode.
   *
   * @return a list of IE-Tree that is the initial population.
   */
  def createInitialPopulation(populationSize: Int, maxDepth: Int, gaussianProbability: Double): List[ImprovedExpressionTree] = {

    /**
     * Inner method that loops over the number of the initial population.
     *
     * @param nTrees the initial population size.
     * @param population an accumulator for the initial population.
     *
     * @return the initial population.
     */
    @tailrec
    def create(nTrees: Int, population: List[ImprovedExpressionTree]) : List[ImprovedExpressionTree] = nTrees match {
      case 0 => population
      case _ => create(nTrees - 1, TreeManager.createRandomTree(maxDepth, gaussianProbability) :: population)
    }

    create(populationSize, Nil)
  }

  /**
   * Static method to create a random tree.
   *
   * @param maxDepth the max depth of the trees.
   * @param gaussianProbability the probability of create a GaussianNode.
   * @param depth the initial depth.
   *
   * @return an IE-Tree randomly created.
   */
  def createRandomTree(maxDepth: Int, gaussianProbability: Double, depth: Int = 0): ImprovedExpressionTree = depth match {
    case d if d >= maxDepth =>
      createTerminal(gaussianProbability)
    case _ =>
      val random = Random.nextInt(FunctionSet.maxId + TerminalSet.maxId)
      if (random > FunctionSet.maxId) {
        this.createTerminal(gaussianProbability)
      }
      else {
        ImprovedExpressionTree(FunctionSet.values.toSeq(random % TerminalSet.maxId),
          createRandomTree(maxDepth, gaussianProbability, depth + 1), createRandomTree(maxDepth, gaussianProbability, depth + 1))
      }
  }

  /**
   * Private method to create a terminal.
   * If the node is a terminal T, then with a probability of gaussianProbability a GaussianNode is generated.
   *
   * @param gaussianProbability the probability of create a GaussianNode.
   *
   * @return the terminal node created.
   */

  private def createTerminal(gaussianProbability: Double): ImprovedExpressionTree = {
    val gaussianProbability = Random.nextDouble()
    if (gaussianProbability < gaussianProbability) {
      ImprovedExpressionTree(TerminalSet.GAUSSIAN, DoubleUtils.getRandomDouble(), DoubleUtils.getRandomDouble(), multiplierFactor = DoubleUtils.getRandomDouble())
    }
    else {
      val terminal = TerminalSet.values.toSeq(Random.nextInt(TerminalSet.maxId - 1))
      terminal match {
        case TerminalSet.CONST => ImprovedExpressionTree(terminal, DoubleUtils.getRandomDouble(), multiplierFactor = DoubleUtils.getRandomDouble())
        case _ => ImprovedExpressionTree(terminal, multiplierFactor = DoubleUtils.getRandomDouble())
      }
    }
  }


   */


}
