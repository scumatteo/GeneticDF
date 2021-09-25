package utils

/**
 * Object that implements utility functions for the GaussianNode.
 */
object GaussianUtils {

  val rad2Pi: Double = math.sqrt(2 * math.Pi)

  /**
   * Static method that returns the ordinate of the Gaussian function in the input value.
   *
   * @param value, the input value.
   * @param mean, the mean of the normal distribution.
   * @param variance, the variance of the normal distribution.
   * @return the ordinate value.
   */
  def getGaussianValue(value: Double, mean: Double, variance: Double): Double = {
    (1 / (math.sqrt(variance) * rad2Pi)) * math.exp(-((value - mean) * (value - mean)) / (2 * variance))
  }

}
