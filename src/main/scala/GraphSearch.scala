import scala.collection.mutable
/**
  * Created by buck on 7/20/16.
  */
object GraphSearch {

  case class GraphSearchResult[NodeType, EdgeType, CostType](path: List[(EdgeType, NodeType)],
                                                             discoveredNode: NodeType,
                                                             parents: Map[NodeType, (EdgeType, NodeType)],
                                                             finalCosts: Map[NodeType, CostType])

  def search[NodeType, EdgeType, CostType](startNode: NodeType,
                                           edgeFunction: NodeType => Map[EdgeType, NodeType],
                                           predicate: NodeType => Boolean,
                                           cost: EdgeType => CostType,
                                           maximumCostOpt: Option[CostType] = None)
                                           (implicit num: Numeric[CostType]): Option[GraphSearchResult[NodeType, EdgeType, CostType]] = {
    val queue = mutable.PriorityQueue[(CostType, NodeType)]((num.fromInt(0), startNode))(
      Ordering.by((y: (CostType, NodeType)) => num.negate(y._1)))

    val parents = mutable.Map[NodeType, (EdgeType, NodeType)]()
    val finalCosts = mutable.Map[NodeType, CostType]()

    while (queue.nonEmpty) {
      val (currentNodeCost, currentNode) = queue.dequeue()

      maximumCostOpt match {
        case None => ()
        case Some(maximumCost) =>
          if (num.gt(currentNodeCost, maximumCost)) {
            return None
          }
      }


      if (!finalCosts.contains(currentNode)) {
        finalCosts(currentNode) = currentNodeCost

        if (predicate(currentNode)) {
          return Some(GraphSearchResult(
            findPath(parents.toMap, currentNode), currentNode, parents.toMap, finalCosts.toMap)
          )
        }

        for ((edge, neighbor) <- edgeFunction(currentNode)) {
          val alternativeCost = num.plus(currentNodeCost, cost(edge))
          val currentCostOpt = finalCosts.get(neighbor)

          // if there's no known path to `neighbor`, or the current cost is greater than `alternativeCost`
          if (currentCostOpt.isEmpty || num.gt(currentCostOpt.get, alternativeCost)) {
            parents(neighbor) = (edge, currentNode)
            queue.enqueue((alternativeCost, neighbor))
          }
        }
      }
    }

    None
  }

  def findPath[NodeType, EdgeType](parents: Map[NodeType, (EdgeType, NodeType)], finishNode: NodeType): List[(EdgeType, NodeType)] = {
    if (parents.contains(finishNode))
      parents(finishNode) :: findPath(parents, parents(finishNode)._2)
    else
      Nil
  }

  def main(args: Array[String]) {
    println(search[Int, Int, Int](1, (x: Int) => Map[Int, Int](1 -> (x + 1), 4 -> (x + 4)), _ == 11, (x) => 1))
  }
}
