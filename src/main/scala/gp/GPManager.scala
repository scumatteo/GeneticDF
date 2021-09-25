package gp

import tree.ImprovedExpressionTree.{ImprovedExpressionTree, Leaf, Node}
import tree.TreeManager

import scala.annotation.tailrec
import scala.util.Random

/*
object CompetitionType extends Enumeration {
  type CompetitionType = Value
  val TOTAL, PARENTAL = Value
}

 */

object GPManager {

  val POPULATION_SIZE: Int = 100
  val MAX_GENERATIONS: Int = 500
  val MAX_TREE_DEPTH: Int = 3
  val GAUSSIAN_PROBABILITY: Double = 0.5

  val CROSSOVER_RATE = 0.5
  val MUTATION_RATE = 0.5

  val ALPHA_WEIGHT = 0.7

  //def geneticProgramming(competitionType: CompetitionType): Unit = {
  def geneticDataFitting(): Unit = {
    //Step 1: Initialization
    val generation = 0
    val inputs: List[Double] = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    //val outputs: List[Double] = List(6, 15, 28, 45, 66, 91, 120, 153, 190) //2x^2 + 3x + 1
    val outputs: List[Double] = List(2, 4, 6, 8, 10, 12, 14, 16, 18) //2x
    //val outputs: List[Double] = List(5, 19, 49, 101, 181, 295, 449, 649, 901) //x^3 + 2x^2 + x + 1
    val initialPopulation = TreeManager.createInitialPopulation(POPULATION_SIZE)

    @tailrec
    def loop(g: Int, population: List[ImprovedExpressionTree]): Unit = g match {
      case g if g == MAX_GENERATIONS =>
        println("Finish!")
        println(population.head)
        for (in <- inputs) {
          println(population.head.eval(in))
        }
      case _ =>
        println("Generation: " + g.toString)

        //Step 2: Crossover and mutations
        val crossoverParents = population.filter(_ => Random.nextDouble() < CROSSOVER_RATE).grouped(2)
          .filter(l => l.size == 2).filter(l => l(0).depth() > 1 && l(1).depth() > 1).map(l => (l(0), l(1))).toList
        val crossoverOffsprings = crossoverParents.map(t => CrossoverManager.performCrossover(t._1, t._2))

        val mutationsParents = population.filter(_ => Random.nextDouble() < MUTATION_RATE).filter(_.depth() > 0)
        val mutationsOffsprings = mutationsParents.map(t => MutationManager.performMutation(t))


        //Step 3: Model evaluation and selection
        val totalPopulation = crossoverOffsprings.flatMap(c => List(c._1, c._2)) ::: mutationsOffsprings ::: population
        val totalErrors = totalPopulation.map(p => computeTotalError(p, inputs, outputs, (i, o) => Math.abs(i - o), l => l.sum / l.size))

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


        println("Recursion")

        loop(g + 1, finalPopulation)

      //TODO vedere se sistemare
      /*
      competitionType match {
        case CompetitionType.TOTAL =>
          val totalPopulation = crossoverOffsprings.flatMap(c => List(c._1, c._2)) ::: mutationsOffsprings ::: population
          val totalErrors = totalPopulation.map(p => computeTotalError(p, inputs, outputs, (i, o) => Math.abs(i - o), l => l.sum / l.size))

          val minError = totalErrors.min
          val maxError = totalErrors.max

          val complexities = totalPopulation.map(computeComplexity)
          val minComplexities = complexities.min
          val maxComplexities = complexities.max

          val normalizedErrors = totalErrors.map(e => normalize(e, maxError, minError))
          val normalizedComplexities = complexities.map(c => normalize(c, maxComplexities, minComplexities))


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

          }).map(c => c._1).take(POPULATION_SIZE)


          println("Recursion")

          loop(g + 1, finalPopulation)

        case _ =>

          val crossoverParentsError = crossoverParents.map(c => (computeTotalError(c._1, inputs, outputs, (i, o) => Math.abs(i - o), l => l.sum / l.size),
            computeTotalError(c._2, inputs, outputs, (i, o) => Math.abs(i - o), l => l.sum / l.size)))

          val crossoverOffspringsError = crossoverOffsprings.map(c => (computeTotalError(c._1, inputs, outputs, (i, o) => Math.abs(i - o), l => l.sum / l.size),
            computeTotalError(c._2, inputs, outputs, (i, o) => Math.abs(i - o), l => l.sum / l.size)))

          val crossoverParentsComplexities = crossoverParents.map(c => (computeComplexity(c._1), computeComplexity(c._2)))

          val crossoverOffspringsComplexities = crossoverOffsprings.map(c => (computeComplexity(c._1), computeComplexity(c._2)))

          val minError = Math.min(crossoverParentsError.map(c => if(c._1 < c._2) c._1 else c._2).min,
            crossoverOffspringsError.map(c => if(c._1 < c._2) c._1 else c._2).min)
          val maxError = Math.max(crossoverParentsError.map(c => if(c._1 > c._2) c._1 else c._2).max,
            crossoverOffspringsError.map(c => if(c._1 > c._2) c._1 else c._2).max)
          val minComplexities = Math.min(crossoverParentsComplexities.map(c => if(c._1 < c._2) c._1 else c._2).min,
            crossoverOffspringsComplexities.map(c => if(c._1 < c._2) c._1 else c._2).min)
          val maxComplexities = Math.max(crossoverParentsComplexities.map(c => if(c._1 < c._2) c._1 else c._2).max,
            crossoverOffspringsComplexities.map(c => if(c._1 < c._2) c._1 else c._2).max)

          val normalizedCrossoverParentsError = crossoverParentsError.map(e => (normalize(e._1, maxError, minError), normalize(e._2, maxError, minError)))
          val normalizedCrossoverOffspringError = crossoverOffspringsError.map(e => (normalize(e._1, maxError, minError), normalize(e._2, maxError, minError)))

          val normalizedCrossoverParentsComplexities = crossoverParentsComplexities.map(c => (normalize(c._1, maxComplexities, minComplexities), normalize(c._2, maxComplexities, minComplexities)))
          val normalizedCrossoverOffspringsComplexities = crossoverOffspringsComplexities.map(c => (normalize(c._1, maxComplexities, minComplexities), normalize(c._2, maxComplexities, minComplexities)))

          val newCrossoverPopulation = crossoverParents.indices.toList.flatMap(i => {
            val newTree1 =
              if(crossoverOffspringsError(i)._1 <= crossoverParentsError(i)._1 &&
              crossoverParentsComplexities(i)._1 <= crossoverParentsComplexities(i)._1) {
                crossoverOffsprings(i)._1
              } else {
              val parentFitness = computeFitness(normalizedCrossoverParentsError(i)._1, normalizedCrossoverParentsComplexities(i)._1)
              val offspringFitness = computeFitness(normalizedCrossoverOffspringError(i)._1, normalizedCrossoverOffspringsComplexities(i)._1)

              if(offspringFitness <= parentFitness) crossoverOffsprings(i)._1 else crossoverParents(i)._1
            }

            val newTree2 =
              if(crossoverOffspringsError(i)._2 <= crossoverParentsError(i)._2 &&
                crossoverParentsComplexities(i)._2 <= crossoverParentsComplexities(i)._2) {
                crossoverOffsprings(i)._2
              } else {
                val parentFitness = computeFitness(normalizedCrossoverParentsError(i)._2, normalizedCrossoverParentsComplexities(i)._2)
                val offspringFitness = computeFitness(normalizedCrossoverOffspringError(i)._2, normalizedCrossoverOffspringsComplexities(i)._2)

                if(offspringFitness <= parentFitness) crossoverOffsprings(i)._2 else crossoverParents(i)._2
              }

            List(newTree1, newTree2)
          })

          val mutationsParentsError = mutationsParents.map(c => computeTotalError(c, inputs, outputs, (i, o) => Math.abs(i - o), l => l.sum / l.size))
          val mutationsOffspringsError = mutationsOffsprings.map(c => computeTotalError(c, inputs, outputs, (i, o) => Math.abs(i - o), l => l.sum / l.size))

          val mutationsParentsComplexities = mutationsParents.map(c => computeComplexity(c))
          val mutationsOffspringsComplexities = mutationsOffsprings.map(c => computeComplexity(c))

          val minMutationsError = Math.min(mutationsParentsError.min, mutationsOffspringsError.min)
          val maxMutationsError = Math.max(mutationsParentsError.max, mutationsOffspringsError.max)
          val minMutationsComplexities = Math.min(mutationsParentsComplexities.min, mutationsOffspringsComplexities.min)
          val maxMutationsComplexities = Math.max(mutationsParentsComplexities.max, mutationsOffspringsComplexities.max)

          val normalizedMutationsParentsError = mutationsParentsError.map(e => normalize(e, maxMutationsError, minMutationsError))
          val normalizedMutationsOffspringError = mutationsOffspringsError.map(e => normalize(e, maxMutationsError, minMutationsError))

          val normalizedMutationsParentsComplexities = mutationsParentsComplexities.map(c => normalize(c, maxMutationsComplexities, minMutationsComplexities))
          val normalizedMutationsOffspringsComplexities = mutationsOffspringsComplexities.map(c => normalize(c, maxMutationsComplexities, minMutationsComplexities))

          val newMutationsPopulation = mutationsParents.indices.toList.map(i => {
            val newTree1 =
              if(mutationsOffspringsError(i) <= mutationsParentsError(i) &&
                mutationsOffspringsComplexities(i) <= mutationsParentsComplexities(i)) {
                mutationsOffsprings(i)
              } else {
                val parentFitness = computeFitness(normalizedMutationsParentsError(i), normalizedMutationsParentsComplexities(i))
                val offspringFitness = computeFitness(normalizedMutationsOffspringError(i), normalizedMutationsOffspringsComplexities(i))

                if(offspringFitness <= parentFitness) mutationsOffsprings(i) else mutationsParents(i)
              }

            newTree1
          })

          val newPop = population.filterNot(e => crossoverParents.flatMap(c => List(c._1, c._2)).contains(e)).filterNot(
            e => mutationsParents.contains(e)) ::: newCrossoverPopulation ::: newMutationsPopulation

          println("Recursion")

          loop(g + 1, newPop)


        //TODO uguale per mutazioni




      }*/


    }

    loop(generation, initialPopulation)


  }

  def computeTotalError(tree: ImprovedExpressionTree, inputs: List[Double], outputs: List[Double],
                        errorFunc: (Double, Double) => Double, totalErrorFunc: List[Double] => Double): Double = {
    val errors = inputs.indices.toList.map(i => {
      errorFunc(tree.eval(inputs(i)), outputs(i))
    })

    totalErrorFunc(errors)
  }

  def computeComplexity(tree: ImprovedExpressionTree): Int = {

    def compute(tree: ImprovedExpressionTree, acc: Int): Int = tree match {
      case _: Leaf => acc + 1
      case n: Node => compute(n.l, acc) + compute(n.r, acc) + 1
    }

    compute(tree, 0)
  }

  def normalize(value: Double, max: Double, min: Double): Double = (value - min) / (max - min)

  def computeFitness(error: Double, complexity: Double, alpha: Double = ALPHA_WEIGHT): Double = {
    alpha * error + (1 - alpha) * complexity
  }

  /*
    def computeFinalPopulation(totalPopulation: List[ImprovedExpressionTree], totalErrors: List[Double],
                               normalizedErrors: List[Double], normalizedComplexities: List[Double],
                               competitionType: CompetitionType): List[ImprovedExpressionTree] = competitionType match {
      case CompetitionType.TOTAL =>
      case _ =>

    }

   */


}
