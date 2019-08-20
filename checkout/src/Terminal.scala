package checkout

case class Product(code: String)
case class Deal(product: Product, count: Int, price: BigDecimal)
case class Receipt(items: Seq[Deal]) {
  def total = items.map(_.price).sum
}

object Terminal {
  private implicit object DealIsKnapsackItem extends KnapsackItem[Deal, BigDecimal] {
    override def value(item: Deal): BigDecimal = item.price
    override def weight(item: Deal): Int       = item.count
  }

  def checkout(prices: Set[Deal], cart: Seq[Product]): Option[Receipt] = {
    val dealsByProduct = prices.groupBy(_.product)
    val productCounts  = cart.groupBy(identity).mapValues(_.size)

    val productReceipts = for {
      (product, count) <- productCounts
    } yield {
      val deals     = dealsByProduct.getOrElse(product, Set.empty)
      val bestDeals = new LeastValueUnboundedKnapsack[Deal, BigDecimal](deals, count).solve()
      (product, bestDeals.map(ds => Receipt(ds.sortBy(_.count).reverse)))
    }

    productReceipts.force.sortBy(_._1.code).map(_._2).foldLeft[Option[Receipt]](Some(Receipt(Seq.empty))) {
      case (Some(r1), Some(r2)) => Some(Receipt(r1.items ++ r2.items))
      case _                    => None
    }
  }
}

class Terminal(prices: Set[Deal] = Set.empty, cart: Seq[Product] = Seq.empty) {
  def setPricing(productCode: String, price: BigDecimal, count: Int = 1): Terminal = {
    new Terminal(prices.incl(Deal(Product(productCode), count, price)), cart)
  }

  def scan(productCode: String): Terminal = {
    new Terminal(prices, cart.appended(Product(productCode)))
  }

  def receipt: Option[Receipt] = {
    Terminal.checkout(prices, cart)
  }

  def total: Option[BigDecimal] = {
    receipt.map(_.total)
  }
}
