package gp

object CompetitionType extends Enumeration {
  type CompetitionType = Value
  val TOTAL, PARENTAL = Value
}

object GPManager {

  val POPULATION_SIZE: Int = 100
  val MAX_GENERATIONS: Int = 500
  val MAX_TREE_DEPTH: Int = 3
  val GAUSSIAN_PROBABILITY: Double = 0.5

  val CROSSOVER_RATE = 0.5
  val MUTATION_RATE = 0.5

  val ALPHA_WEIGHT = 0.7


  def geneticProgramming(): Unit = ???




}
