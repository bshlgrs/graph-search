import scala.collection.mutable

/**
  * Created by buck on 7/20/16.
  */

// start 8:51

object EditDistance {
  def distance[A](list1: List[A], list2: List[A]): Int = {
    val d = mutable.Map[(Int, Int), Int]((0, 0) -> 0)

    1.to(list1.length).foreach((n) => {
      d((n, 0)) = n
    })

    1.to(list2.length).foreach((n) => {
      d((0, n)) = n
    })

    for (j <- 1.to(list2.length - 1)) {
      for (i <- 1.to(list1.length - 1)) {
        if (list1(i) == list2(j)) {
          d((i, j)) = d((i - 1, j - 1))
        } else {
          d((i, j)) = List(
            d(i - 1, j) + 1,
            d(i, j - 1) + 1,
            d(i - 1, j - 1) + 1
          ).min
        }
      }
    }

    d((list1.length - 1, list2.length - 1))
  }

  def main(args: Array[String]) {
    println(distance(List(1,2,3,4,5), List(2,3,4,5)))
  }
}
