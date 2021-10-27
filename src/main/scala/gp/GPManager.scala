package gp

import tree.ImprovedExpressionTree.{GaussianNode, ImprovedExpressionTree, InputNode, Leaf, Node}
import tree.TreeManager

import scala.annotation.tailrec
import scala.util.Random

/**
 * Object to manage the fundamental part of genetic programming.
 */
object GPManager {

  /**
   * The maximum population size.
   */
  val POPULATION_SIZE: Int = 200

  /**
   * The maximum number of generations.
   */
  val MAX_GENERATIONS: Int = 500

  /**
   * The maximum depth of the trees.
   */
  val MAX_TREE_DEPTH: Int = 15

  /**
   * The probability with which a terminal is generated as a GaussianNode.
   */
  val GAUSSIAN_PROBABILITY: Double = 0.5

  /**
   * The probability of crossover.
   */
  val CROSSOVER_RATE = 0.5

  /**
   * The probability of carrying out a mutation.
   */
  val MUTATION_RATE = 0.5

  /**
   * Value used as weight in the fitness calculation.
   * The greater it is, the more the error has weight with respect to the complexity of the tree.
   */
  val ALPHA_WEIGHT = 0.99

  /**
   * Static method to perform the genetic data fitting.
   *
   * @param inputs  the list of inputs.
   * @param outputs the list of outputs
   * @return a tuple that contains the best IE-Tree and its error.
   */
  def geneticDataFitting(inputs: List[Double], outputs: List[Double]): (ImprovedExpressionTree, Double) = {

    //Step 1: Initialization
    val generation = 0
    val initialPopulation = TreeManager.createInitialPopulation(POPULATION_SIZE)

    /**
     * Inner function to loop on the generations.
     *
     * @param g          the current generation.
     * @param population the current population.
     * @param bestTreeError  the best error of the previous generation.
     * @return a tuple that contains the best IE-Tree and the best error.
     */
    @tailrec
    def loop(g: Int, population: List[ImprovedExpressionTree], bestTreeError: Double): (ImprovedExpressionTree, Double) = g match {
      case g if g == MAX_GENERATIONS =>
        println("Finish!\n")
        (population.head, bestTreeError)
      case _ =>
        println("Generation: " + (g + 1))

        //Step 2: Crossover and mutations
        val crossoverParents = population.filter(_ => Random.nextDouble() < CROSSOVER_RATE).grouped(2)
          .filter(_.size == 2).filter(l => l(0).depth() > 0 && l(1).depth() > 0).map(l => (l(0), l(1))).toList
        val crossoverOffsprings = crossoverParents.map(t => CrossoverManager.performCrossover(t._1, t._2))

        val mutationsParents = population.filter(_ => Random.nextDouble() < MUTATION_RATE)
        val mutationsOffsprings = mutationsParents.map(MutationManager.performMutation)


        //Step 3: Model evaluation and selection
        val totalPopulation = crossoverOffsprings.flatMap(c => List(c._1, c._2)) ::: mutationsOffsprings ::: population
        val totalErrors = totalPopulation.map(computeTotalError(_, inputs, outputs, (i, o) => Math.abs(i - o), l => l.sum / l.size))

        //if error = 0 is found, returns.
        if (totalErrors.indices.exists(totalErrors(_) == 0)) {
          return totalErrors.indices.filter(totalErrors(_) == 0).sortWith((i, j) =>
            computeComplexity(totalPopulation(i)) < computeComplexity(totalPopulation(j)))
            .map(i => (totalPopulation(i), totalErrors(i))).head
        }

        val complexities = totalPopulation.map(computeComplexity)
        val weightedErrors = totalErrors.indices.map(i => computeWeightedError(totalErrors(i), complexities(i)))

        //Step 4: Population for the next generation
        val finalPopulation = totalPopulation.zipWithIndex.sortWith((t1, t2) => {
          val i = t1._2
          val j = t2._2

          weightedErrors(i) < weightedErrors(j)

        }).map(_._1).take(POPULATION_SIZE)

        //Step 5: Loop with current generation + 1
        loop(g + 1, finalPopulation, totalErrors.indices.sortWith((i, j) => weightedErrors(i) < weightedErrors(j)).map(totalErrors(_)).head)

    }

    loop(generation, initialPopulation, 0)

  }

  /**
   * Private static method to compute the total errors of a tree.
   *
   * @param tree           the tree to evaluate.
   * @param inputs         the list of inputs.
   * @param outputs        the list of outputs.
   * @param errorFunc      the error function to calculate a single error.
   * @param totalErrorFunc the error function to accumulate the single errors.
   * @return the total error of a tree, evaluated on all the inputs.
   */
  private def computeTotalError(tree: ImprovedExpressionTree, inputs: List[Double], outputs: List[Double],
                                errorFunc: (Double, Double) => Double, totalErrorFunc: List[Double] => Double): Double = {
    val errors = inputs.indices.toList.map(i => {
      errorFunc(tree.eval(inputs(i)), outputs(i))
    })

    totalErrorFunc(errors)
  }

  /**
   * Private static method to compute the complexity of a tree. It's computed as the number of nodes of the tree.
   *
   * @param tree the tree for which compute the complexity.
   * @return the complexity of the tree.
   */
  private def computeComplexity(tree: ImprovedExpressionTree): Int = {

    def compute(tree: ImprovedExpressionTree, complexity: Int): Int = tree match {
      case _: Leaf => complexity + 1
      case n: Node => compute(n.l, complexity) + compute(n.r, complexity) + 1
    }

    compute(tree, 0)
  }

  /**
   * Private static method to compute the weighted error of a tree, balancing the error and the complexity.
   *
   * @param error      the error made by the tree.
   * @param complexity the complexity of the tree.
   * @param alpha      a weight to balance the complexity and the error.
   * @return the weighted error of the tree.
   */
  private def computeWeightedError(error: Double, complexity: Double, alpha: Double = ALPHA_WEIGHT): Double = {
    alpha * error + (1 - alpha) * complexity
  }

}
