package checkout

/**
  * This is the implementation of the unbounded knapsack algorithm from https://en.wikipedia.org/wiki/Knapsack_problem,
  * modified to compute least value combination at full capacity.
  */
class LeastValueUnboundedKnapsack[I, V](availableItems: Set[I], capacity: Int)(
    implicit evki: KnapsackItem[I, V],
    evvn: Numeric[V],
) {
  import evki.{value => itemValue, weight => itemWeight}, evvn._

  private sealed trait Solution
  private case class Knapsack(value: V, items: List[I]) extends Solution {
    def addItem(item: I): Knapsack =
      Knapsack(value + itemValue(item), item :: items)
  }
  private case object NoSolution extends Solution

  private def memoize[I, O](f: I => O): I => O = {
    val cache = collection.mutable.HashMap.empty[I, O]
    (i: I) => cache.getOrElseUpdate(i, f(i))
  }

  private lazy val solveForCapacity: Int => Either[NoSolution.type, Knapsack] = memoize { capacity =>
    if (capacity == 0) {
      Right(Knapsack(zero, List.empty))
    } else {
      val itemsThatFit = availableItems.filter(i => itemWeight(i) <= capacity)
      val possibleKnapsacks = itemsThatFit
        .map(item => {
          val remainingCapacity = capacity - itemWeight(item)
          val remainingSolution = solveForCapacity(remainingCapacity)
          remainingSolution.map(_.addItem(item))
        })
        .collect {
          case Right(k) => k
        }
      possibleKnapsacks.minByOption(_.value).toRight(NoSolution)
    }
  }

  def solve(): Option[List[I]] = {
    solveForCapacity(capacity).toOption.map(_.items)
  }
}

trait KnapsackItem[I, V] {
  def weight(item: I): Int
  def value(item: I): V
}
