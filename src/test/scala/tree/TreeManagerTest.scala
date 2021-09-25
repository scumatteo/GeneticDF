package tree

import gp.GPManager
import org.scalatest.funsuite.AnyFunSuite

class TreeManagerTest extends AnyFunSuite {

  val populationSize: Int = 50

  test(s"The method createInitialPopulation should return a list of IE-Tree of size equals to populationSize"){
    assert(TreeManager.createInitialPopulation(populationSize).size == populationSize)
  }

  test(s"The method createRandomTree should create a random IE-Tree of depth less or equal to maxDepth"){
    assert(TreeManager.createRandomTree().depth() <= GPManager.MAX_TREE_DEPTH)
  }

}
