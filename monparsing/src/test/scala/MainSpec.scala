import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._

class MainSpec extends AnyFlatSpec with should.Matchers {
  import Parsers._

  "result" should "lift a value into a parser" in {
    result("hello")("world") should be(List(("hello", "world")))
  }

  "zero" should "always fail" in {
    zero("hello") should be(List())
  }

  "item" should "consume the first character in the input string" in {
    item("hello") should be(List(('h', "ello")))
    item("h") should be(List(('h', "")))
    item("") should be(List())
  }

  "bind" should "pass results to second parser and first parser succeeds" in {
    bind(item)(a => result(a.toUpper))("hello") should be(List(('H', "ello")))
    bind(item)(a => result(a.toUpper))("") should be(List())
  }
}
