package checkout.tests

import checkout._
import utest._

object TerminalTests extends TestSuite {

  val term = new Terminal()
    .setPricing("A", 2)
    .setPricing("A", 7, 4)
    .setPricing("B", 12)
    .setPricing("C", 1.25)
    .setPricing("C", 6, 6)
    .setPricing("D", .15)

  def assertTotal(cart: String, expectedTotal: Option[BigDecimal], terminal: Terminal = term): Unit = {
    val r = cart.foldLeft(terminal)((t, p) => t.scan(p.toString)).receipt
    assert(r.map(_.total) == expectedTotal)
  }

  val tests = Tests {
    test("Cart ABCDABAA") {
      assertTotal("ABCDABAA", Some(32.40))
    }
    test("Cart CCCCCCC") {
      assertTotal("CCCCCCC", Some(7.25))
    }
    test("Cart ABCD") {
      assertTotal("ABCD", Some(15.40))
    }
    test("Empty cart") {
      assertTotal("", Some(0))
    }
    test("Impossible cart") {
      assertTotal("BB", None, new Terminal().setPricing("B", 1, 3))
    }
  }
}
