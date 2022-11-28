package aoc22.common

import cats.parse.{Parser, Parser0, Numbers}

object CommonParsers:
  val spaces: Parser[Any] = Parser.char(' ').rep

  val newLine: Parser0[Any] = Parser.char('\r').? ~ Parser.char('\n')

  val int: Parser[Int] = Numbers.signedIntString.map(_.toInt)

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