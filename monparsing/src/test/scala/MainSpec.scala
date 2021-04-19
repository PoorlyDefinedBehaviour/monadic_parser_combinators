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

  ">>=" should "pass results to second parser if the first parser succeeds" in {
    val parser = item >>= (a => result(a.toUpper))

    parser("hello") should be(List(('H', "ello")))
    parser("") should be(List())

    (zero >>= result)("hello") should be(List())
  }

  "sat" should "succeed when the first character from the input string passes the predicate" in {
    sat(_ == 'b')("abc") should be(List())
    sat(_ == 'a')("") should be(List())
    sat((_) => true)("") should be(List())
    sat(_ == 'a')("abc") should be(List(('a', "bc")))
  }

  "char" should "succeed when the first character from the input string is the expected character" in {
    char('a')("") should be(List())
    char('a')("bc") should be(List())
    char('a')("abc") should be(List(('a', "bc")))
  }

  "digit" should "succeed when the first character from the input string is a digit" in {
    digit("") should be(List())
    digit("a1") should be(List())
    digit("1a") should be(List(('1', "a")))
    digit("1") should be(List(('1', "")))
    digit("12345") should be(List(('1', "2345")))
  }

  "lower" should "succeed when the first character from the input string is in lower case" in {
    lower("") should be(List())
    lower("A") should be(List())
    lower("b") should be(List(('b', "")))
  }

  "upper" should "succeed when the first character from the input string is in upper case" in {
    upper("") should be(List())
    upper("b") should be(List())
    upper("A") should be(List(('A', "")))
  }

  "++" should "return the result of both parsers" in {
    (upper ++ lower)("Abc") should be(List(('A', "bc")))
    (lower ++ upper)("Abc") should be(List(('A', "bc")))
    (digit ++ digit)("123") should be(List(('1', "23"), ('1', "23")))
    (digit ++ digit)("") should be(List())
    (digit ++ digit)("abc") should be(List())
  }
}
