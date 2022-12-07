package aoc22.day7

import aoc22.common.CommonParsers
import cats.parse.Parser
import scala.collection.mutable.ArrayBuffer


enum ListingEntry:
  case Directory(name: String)
  case File(name: String, size: Int)

enum TerminalCommand:
  case ListCmd(entries: List[ListingEntry])
  case ChangeDir(dest: String)

object Parsing:
  def filenameParser: Parser[String] =
    (CommonParsers.alphanum | CommonParsers.char('.')).rep(1).map(_.toList.mkString)

  def directoryParser: Parser[String] =
    CommonParsers.string("..")
      // Somewhat too lenient filepath parser
      | (CommonParsers.alphanum | CommonParsers.char('/')).rep(1).map(_.toList.mkString)

  def listingDirParser: Parser[ListingEntry.Directory] = for
    _ <- Parser.string("dir ")
    name <- CommonParsers.alphanum.rep(1).map(_.toList.mkString)
  yield
    ListingEntry.Directory(name)

  def listingFileParser: Parser[ListingEntry.File] =
    ((CommonParsers.int <* Parser.char(' ')) ~ filenameParser)
      .map { case (size, name) => ListingEntry.File(name, size) }

  def listingEntryParser: Parser[ListingEntry] =
    listingDirParser | listingFileParser

  def changeDirParser: Parser[TerminalCommand.ChangeDir] =
    (Parser.string("cd ") *> directoryParser).map(TerminalCommand.ChangeDir.apply)

  def lsParser: Parser[TerminalCommand.ListCmd] = for
    _ <- Parser.string("ls")
    _ <- CommonParsers.newLine
    output <- CommonParsers.lineSeparated(listingEntryParser)
  yield
    TerminalCommand.ListCmd(output)

  def commandParser: Parser[TerminalCommand] =
    Parser.string("$ ") *> (changeDirParser | lsParser)

  def terminalOutputParser: Parser[List[TerminalCommand]] = CommonParsers.lineSeparated(commandParser)
