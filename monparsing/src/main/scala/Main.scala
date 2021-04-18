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
}

object Main extends App {
  println("Hello, World!")
}
