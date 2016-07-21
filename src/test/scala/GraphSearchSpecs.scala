import org.scalatest._

class GraphSearchSpecs extends FlatSpec with Matchers {

  "GraphSearch" should "correctly solve a Nim-style problem" in {
    val graphSearchResult = GraphSearch.search[Int, Int, Int](1, (x: Int) => Map[Int, Int](1 -> (x + 1), 4 -> (x + 4)), _ == 11, (x) => 1)
    graphSearchResult.get.discoveredNode should be(11)
    graphSearchResult.get.finalCosts(11) should be(4)
  }

}
