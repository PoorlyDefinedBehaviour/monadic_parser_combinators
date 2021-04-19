object Parsers {
  type Parser[A] = String => List[(A, String)]

  def result[A](a: A): Parser[A] = input => List((a, input))

  def zero[A]: Parser[A] = input => List()

  def item: Parser[Char] = input =>
    if (input.isEmpty()) {
      List()
    } else {
      List((input.head, input.tail))
    }

  def sat(predicate: (Char => Boolean)): Parser[Char] =
    item >>= (character =>
      if (predicate(character)) result(character) else zero
    )

  def char(character: Char): Parser[Char] =
    sat(_ == character)

  def digit: Parser[Char] =
    sat(character => character >= '0' && character <= '9')

  def lower: Parser[Char] =
    sat(character => character >= 'a' && character <= 'z')

  def upper: Parser[Char] =
    sat(character => character >= 'A' && character <= 'Z')

  def letter: Parser[Char] =
    lower ++ upper

  implicit class ParserOperators[A](parser: Parser[A]) {
    def >>=[B](f: A => Parser[B]): Parser[B] =
      input =>
        parser(input).flatMap((parserAResult) => {
          val (parsed, restOfInput) = parserAResult
          f(parsed)(restOfInput)
        })

    def ++(parserTwo: Parser[A]): Parser[A] =
      input => List.concat(parser(input), parserTwo(input))
  }
}

object Main extends App {
  println("Hello, World!")
}
