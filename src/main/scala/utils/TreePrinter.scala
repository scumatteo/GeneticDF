package utils

import tree.ImprovedExpressionTree.{ImprovedExpressionTree, Node}

import scala.collection.immutable.SortedMap

object TreePrinter {

  val maxWidth = 20

  case class PrintableTree(private val _multiplierFactor: Double, structure: String) {
    val multiplierFactor: String = _multiplierFactor.toString.take(3)

    override def toString: String = {
      s"PrintableTree($multiplierFactor, $structure)"
    }
  }
  /*
    def printTree(improvedExpressionTree: ImprovedExpressionTree): Unit = {
      var flattenedTree: SortedMap[Int, List[PrintableTree]] = SortedMap(0 -> List(PrintableTree(
        improvedExpressionTree.multiplierFactor,
        improvedExpressionTree.label())
      ))
      for(i <- 1 until improvedExpressionTree.depth()) flattenedTree = flattenedTree + (i -> improvedExpressionTree.getNodesAtDepth(i).map(t => PrintableTree(t.multiplierFactor, t.label())).toList)
      print(flattenedTree)
    }
  */

  def printTree(improvedExpressionTree: ImprovedExpressionTree): Unit = {
    val treeToPrint: SortedMap[Int, Seq[Option[PrintableTree]]] = flattenTree(improvedExpressionTree)
    println(treeToPrint)
    //printLevel(treeToPrint)
    for (i <- 0 until treeToPrint.size) printLevel(treeToPrint, i)
  }

  private def flattenTree(improvedExpressionTree: ImprovedExpressionTree, depth: Int = 0): SortedMap[Int, Seq[Option[PrintableTree]]] = {
    var flattenedTree: SortedMap[Int, List[Option[PrintableTree]]] = SortedMap(0 -> List(Some(PrintableTree(
      improvedExpressionTree.multiplierFactor,
      improvedExpressionTree.label())
    )))
    for (i <- 1 until improvedExpressionTree.depth()) flattenedTree = flattenedTree + (i -> List.empty)

    def flatten(improvedExpressionTree: ImprovedExpressionTree, depth: Int): Unit = improvedExpressionTree match {
      case n: Node =>
        val l = n.l
        val r = n.r
        val treeSequencel = Some(PrintableTree(l.multiplierFactor, l.label())) :: flattenedTree(depth)
        flattenedTree = flattenedTree + (depth -> treeSequencel)
        flatten(l, depth + 1)

        val treeSequencer = Some(PrintableTree(r.multiplierFactor, r.label())) :: flattenedTree(depth)
        flattenedTree = flattenedTree + (depth -> treeSequencer)
        flatten(r, depth + 1)

      case _ if depth < flattenedTree.size =>
        val treeSequence = None :: flattenedTree(depth)
        flattenedTree = flattenedTree + (depth -> treeSequence)

      case _ =>

    }

    flatten(improvedExpressionTree, depth + 1)
    flattenedTree

  }

  private def printLevel(flattenedTree: SortedMap[Int, Seq[Option[PrintableTree]]], depth: Int = 0): Unit = {

    val initialSpace: String = "                " * (flattenedTree.size - depth - 1)

    for (i <- flattenedTree(depth).indices) {
      if (i == 0) print(initialSpace)
      flattenedTree(depth)(i) match {
        case None => print(" " * maxWidth)
        case Some(n) =>
          val mp = n.multiplierFactor

          print("mp: " + mp + "        " + " " * (maxWidth - mp.length) + " " * (flattenedTree.size - depth - 1))


      }
    }
    println("")

    for (i <- flattenedTree(depth).indices) {
      if (i == 0) {
        print(initialSpace)
      }
      flattenedTree(depth)(i) match {
        case None => print(" " * maxWidth)
        case Some(n) =>
          val sp = n.structure

          print("sp: " + sp + "        " + " " * (maxWidth - sp.length) + " " * (flattenedTree.size - depth - 1))


      }

    }

    println("")




  }



}

