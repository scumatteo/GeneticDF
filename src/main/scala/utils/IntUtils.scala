package utils

import scala.util.Random

/**
 * Object that implements utility function for Integers.
 */
object IntUtils {

  /**
   * Static method that returns a random integer between min (inclusive) and max (exclusive).
   *
   * @param min the inferior limit of the range.
   * @param max the superior limit of the range.
   * @return the random integer inside the range.
   */
  def getRandomInt(min: Int, max: Int): Int = Random.nextInt(Math.max(2, max - min)) + min

}
