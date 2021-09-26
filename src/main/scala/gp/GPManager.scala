package gp

import tree.ImprovedExpressionTree.{ImprovedExpressionTree, Leaf, Node}
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
  val ALPHA_WEIGHT = 0.7

  /**
   * Static method to perform the genetic data fitting.
   *
   * @param inputs the list of inputs.
   * @param outputs the list of outputs
   *
   * @return a tuple that contains the best IE-Tree and its error.
   */
  def geneticDataFitting(inputs: List[Double], outputs: List[Double]): (ImprovedExpressionTree, Double) = {

    //Step 1: Initialization
    val generation = 0
    val initialPopulation = TreeManager.createInitialPopulation(POPULATION_SIZE)

    /**
     * Inner function to loop on the generations.
     *
     * @param g the current generation.
     * @param population the current population.
     * @param bestError the best error of the previous generation.
     *
     * @return a tuple that contains the best IE-Tree and the best error.
     */
    @tailrec
    def loop(g: Int, population: List[ImprovedExpressionTree], bestError: Double): (ImprovedExpressionTree, Double) = g match {
      case g if g == MAX_GENERATIONS =>
        println("Finish!\n")
        (population.head, bestError)
      case _ =>
        println("Generation: " + (g + 1))

        //Step 2: Crossover and mutations
        val crossoverParents = population.filter(_ => Random.nextDouble() < CROSSOVER_RATE).grouped(2)
          .filter(l => l.size == 2).filter(l => l(0).depth() > 0 && l(1).depth() > 0).map(l => (l(0), l(1))).toList
        val crossoverOffsprings = crossoverParents.map(t => CrossoverManager.performCrossover(t._1, t._2))

        val mutationsParents = population.filter(_ => Random.nextDouble() < MUTATION_RATE)
        val mutationsOffsprings = mutationsParents.map(t => MutationManager.performMutation(t))


        //Step 3: Model evaluation and selection
        val totalPopulation = crossoverOffsprings.flatMap(c => List(c._1, c._2)) ::: mutationsOffsprings ::: population
        val totalErrors = totalPopulation.map(p => computeTotalError(p, inputs, outputs, (i, o) => Math.abs(i - o), l => l.sum / l.size))

        if (totalErrors.indices.exists(i => totalErrors(i) == 0)) {
          return totalErrors.indices.filter(i => totalErrors(i) == 0).map(i => (totalPopulation(i), totalErrors(i))).head
        }

        val minError = totalErrors.min
        val maxError = totalErrors.max

        val complexities = totalPopulation.map(computeComplexity)
        val minComplexities = complexities.min
        val maxComplexities = complexities.max

        val normalizedErrors = totalErrors.map(e => normalize(e, maxError, minError))
        val normalizedComplexities = complexities.map(c => normalize(c, maxComplexities, minComplexities))

        /*
         val finalPopulation = totalPopulation.zipWithIndex.sortWith((t1, t2) => {
             val i = t1._2
             val j = t2._2


             val fitness1 = computeFitness(normalizedErrors(i), normalizedComplexities(i))
              val fitness2 = computeFitness(normalizedErrors(j), normalizedComplexities(j))
              if (fitness1 < fitness2) true else false

           }).map(_._1).take(POPULATION_SIZE)

         */

        //Step 4: Population for the next generation
        val finalPopulation = totalPopulation.zipWithIndex.sortWith((t1, t2) => {
          val i = t1._2
          val j = t2._2

          if (totalErrors(i) < totalErrors(j)) true
          else if (totalErrors(i) > totalErrors(j)) false
          else {
            val fitness1 = computeFitness(normalizedErrors(i), normalizedComplexities(i))
            val fitness2 = computeFitness(normalizedErrors(j), normalizedComplexities(j))
            if (fitness1 < fitness2) true
            else false
          }

        }).map(_._1).take(POPULATION_SIZE)

        //Step 5: Loop with current generation + 1
        loop(g + 1, finalPopulation, totalErrors.sortWith((e1, e2) => e1 < e2).head)

    }

    loop(generation, initialPopulation, 0)

  }

  /**
   * Private static method to compute the total errors of a tree.
   *
   * @param tree the tree to evaluate.
   * @param inputs the list of inputs.
   * @param outputs the list of outputs.
   * @param errorFunc the error function to calculate a single error.
   * @param totalErrorFunc the error function to accumulate the single errors.
   *
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
   *
   * @return the complexity of the tree.
   */
  private def computeComplexity(tree: ImprovedExpressionTree): Int = {

    def compute(tree: ImprovedExpressionTree, acc: Int): Int = tree match {
      case _: Leaf => acc + 1
      case n: Node => compute(n.l, acc) + compute(n.r, acc) + 1
    }

    compute(tree, 0)
  }

  /**
   * Private static method to normalize a value.
   *
   * @param value the value to normalize.
   * @param max the max value of the range.
   * @param min the min value of the range.
   *
   * @return the normalized value.
   */
  private def normalize(value: Double, max: Double, min: Double): Double = (value - min) / (max - min)

  /**
   * Private static method to compute the fitness of a tree.
   *
   * @param error the error made by the tree.
   * @param complexity the complexity of the tree.
   * @param alpha a weight to balance the complexity and the error.
   *
   * @return the fitness of the tree.
   */
  private def computeFitness(error: Double, complexity: Double, alpha: Double = ALPHA_WEIGHT): Double = {
    alpha * error + (1 - alpha) * complexity
  }

}
