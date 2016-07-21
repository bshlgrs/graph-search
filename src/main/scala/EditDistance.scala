import scala.collection.mutable

/**
  * Created by buck on 7/20/16.
  */

// start 8:51

object EditDistance {
  abstract class SubstitutionCost[A] {
    def cost(x: A, y: A): Option[Int]
  }

  case class ConstantSubstitutionCost[A](constantCost: Int) extends SubstitutionCost[A] {
    def cost(x: A, y: A) = Some(constantCost)
  }

  class UnitSubstitutionCost[A] extends SubstitutionCost[A] {
    def cost(x: A, y: A) = Some(1)
  }

  def distance[TokenType](xs: List[TokenType],
                          ys: List[TokenType],
                          insertionCost: Option[Int] = Some(1),
                          deletionCost: Option[Int] = Some(1),
                          substitutionCost: SubstitutionCost[TokenType] = new UnitSubstitutionCost): Int = {
    val d = mutable.Map[(Int, Int), Int]((0, 0) -> 0)

    1.to(xs.length).foreach((n) => {
      d((n, 0)) = n
    })

    1.to(ys.length).foreach((n) => {
      d((0, n)) = n
    })

    for (j <- 1.to(ys.length - 1)) {
      for (i <- 1.to(xs.length - 1)) {
        val x = xs(i)
        val y = ys(j)
        if (x == y) {
          d((i, j)) = d((i - 1, j - 1))
        } else {
          d((i, j)) = List(
            insertionCost.map(_ + d(i - 1, j)),
            deletionCost.map(_ + d(i, j - 1)),
            substitutionCost.cost(x, y).map(_ + d(i - 1, j - 1))
          ).flatten.min
        }
      }
    }

    d((xs.length - 1, ys.length - 1))
  }

  def main(args: Array[String]) {
    println(distance(List(2,6,4,5), List(2,3,4,5, 6, 7), Some(1), Some(1), new UnitSubstitutionCost[Int]))
  }
}
