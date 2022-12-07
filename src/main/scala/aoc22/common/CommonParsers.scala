package aoc22.common

import cats.parse.{Parser, Parser0, Numbers, Rfc5234}

object CommonParsers:
  val spaces: Parser[Any] = Parser.char(' ').rep

  val newLine: Parser0[Any] = Parser.char('\r').? ~ Parser.char('\n')

  val blankLine: Parser0[Any] = newLine ~ newLine

  val int: Parser[Int] = Numbers.signedIntString.map(_.toInt)

  val alphanum: Parser[Char] = Rfc5234.alpha | Rfc5234.digit

  def char(char: Char): Parser[Char] =
    Parser.char(char).map(_ => char)

  def string(string: String): Parser[String] =
    Parser.string(string).map(_ => string)

  def pair[A, B](a: Parser[A], b: Parser[B], sep: Parser0[Any]): Parser[(A, B)] =
    (a <* sep) ~ b
  
  def separated[A](p: Parser[A], sep: Parser0[Any]): Parser[List[A]] =
    p.repSep(1, sep).map(_.toList)

  def commaSeparated[A](p: Parser[A]): Parser[List[A]] =
    separated(p, Parser.char(','))

  def spaceSeparated[A](p: Parser[A]): Parser[List[A]] =
    separated(p, spaces)

  def lineSeparated[A](p: Parser[A]): Parser[List[A]] =
    separated(p, newLine)

  def withTrimmedStartingSpaces[A](p: Parser[A]): Parser[A] =
    (spaces.?).with1 *> p