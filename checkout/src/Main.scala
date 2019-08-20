package checkout

object app {

  def main(args: Array[String]): Unit = {
    println("running demo:")

    val term = new Terminal()
      .setPricing("A", 2)
      .setPricing("A", 7, 4)
      .setPricing("B", 12)
      .setPricing("C", 1.25)
      .setPricing("C", 6, 6)
      .setPricing("D", .15)

    for (cart <- Seq("ABCDABAA", "CCCCCCC", "ABCD")) {
      println(s"Cart '$cart':")
      cart.foldLeft(term)((t, p) => t.scan(p.toString)).receipt match {
        case Some(receipt) =>
          println("Receipt:")
          receipt.items.foreach(d => println(s"${d.product.code}: ${d.count} for $$${d.price}"))
          println(s"Total: $$${receipt.total}")
        case None =>
          println("Product selection is impossible for this cart.")
      }
    }
  }
}
