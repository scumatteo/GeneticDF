package utils

import scala.util.Random

/**
 * Object that implements utility function for Doubles.
 */
object DoubleUtils {

  /**
   * Static method that return a random Double between 0 and max.
   *
   * @param max, the max value of the Double.
   * @return the random Double.
   */
  def getRandomDouble(max: Double = 10): Double = Random.nextDouble() * max

}


