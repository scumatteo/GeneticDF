package utils

/**
 * Object that implements utility function for Option.
 */
object OptionUtils {

  /**
   * Implicit class that enriches optional.
   *
   * @param base the optional.
   * @tparam A the type of the optional.
   */
  implicit class MyOption[A](base: Option[A]){

    /**
     * Method that returns the first defined value given two optionals. If no one is defined, it returns None.
     * @param elem the second optional.
     * @return the the first defined Option if it exists, None otherwise.
     */
    def firstDefined(elem: Option[A]): Option[A] = {
      if(base.isDefined) base else if(elem.isDefined) elem else None
    }
  }

}
