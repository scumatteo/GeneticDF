import gp.GPManager
import tree._

object Main {

  def main(args: Array[String]): Unit = {

    val inputs: List[Double] = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    //val outputs: List[Double] = List(6, 15, 28, 45, 66, 91, 120, 153, 190) //2x^2 + 3x + 1
    //val outputs: List[Double] = List(2, 4, 6, 8, 10, 12, 14, 16, 18) //2x
    val outputs: List[Double] = List(5, 19, 49, 101, 181, 295, 449, 649, 901) //x^3 + 2x^2 + x + 1

    val result = GPManager.geneticDataFitting(inputs, outputs)

    val bestTree = result._1
    val bestError = result._2

    println("Best tree: " + bestTree)
    println("Depth: " + bestTree.depth())
    println("Best error: " + bestError)

    for (in <- inputs) {
      println(bestTree.eval(in))
    }


  }
}
