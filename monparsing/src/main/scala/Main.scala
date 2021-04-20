object Parsers {
  type Parser[A] = String => List[(A, String)]

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

  def alphanum: Parser[Char] = letter ++ digit

  def word: Parser[String] = {
    val buildWordFromLetters =
      letter >>= (character => word >>= (string => result(character + string)))

    buildWordFromLetters ++ result("")
  }

  def result[A](a: A): Parser[A] = input => List((a, input))

  def string(str: String): Parser[String] =
    if (str.isEmpty()) {
      result("")
    } else {
      char(str.head) >>= (_ => string(str.tail)) >>= (_ => result(str))
    }

  def map2[A, B, C](parserA: Parser[A], parserB: => Parser[B])(
      f: (A, B) => C
  ): Parser[C] =
    parserA >>= (a => parserB >>= (b => result(f(a, b))))

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) ++ result(List())

  def many1[A](parser: Parser[A]): Parser[List[A]] =
    input => {
      val result = many(parser)(input)

      result.dropRight(1)
    }

  def ident: Parser[String] =
    many(letter).map(_.mkString)

  implicit class ParserMonoidOps[A](parser: Parser[A]) {
    def ++(parserTwo: Parser[A]): Parser[A] =
      input => List.concat(parser(input), parserTwo(input))
  }

  implicit class ParserFunctorOps[A](parser: Parser[A]) {
    def map[B](f: A => B): Parser[B] =
      parser >>= (a => result(f(a)))
  }

  implicit class ParserMonadOps[A](parser: Parser[A]) {
    def >>=[B](f: A => Parser[B]): Parser[B] =
      input =>
        parser(input).flatMap((parserAResult) => {
          val (parsed, restOfInput) = parserAResult
          f(parsed)(restOfInput)
        })
  }
}

object Main extends App {
  println("Hello, World!")
}
