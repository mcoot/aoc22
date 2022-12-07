package aoc22.day7

import scala.collection.mutable.{ArrayBuffer}
import aoc22.common.SolutionWithParser
import cats.parse.Parser

import scala.collection.mutable

sealed trait FileNode:
  def isDirectory: Boolean
  def name: String
  def size: Int


object FNode:
  case class File(filename: String, fileSize: Int) extends FileNode:
    override def isDirectory: Boolean = false
    override def name: String = filename
    override def size: Int = fileSize

  class Directory(
      var parent: Option[FNode.Directory],
      val dirname: String,
      var subdirs: List[FNode.Directory],
      var files: List[FNode.File]
  ) extends FileNode:
    def children: List[FileNode] = files ++ subdirs

    override def isDirectory: Boolean = true
    override def name: String = dirname
    override def size: Int = children.map(_.size).sum

    def isRoot: Boolean = parent.isEmpty

    def hasChild(n: String): Boolean = children.map(_.name).contains(n)

    def navigateToParent: FNode.Directory =
      if isRoot then this else parent.get

    def navigateToRoot: FNode.Directory =
      if isRoot then this else parent.get.navigateToRoot

    def getSubdirByName(dir: String): Option[FNode.Directory] = subdirs.find(_.name == dir)

    def addFile(filename: String, size: Int): Unit =
      if !hasChild(filename) then
        files = FNode.File(filename, size) :: files

    def addSubdir(dir: String): Unit =
      if !hasChild(dir) then
        subdirs = FNode.Directory(Some(this), dir, List(), List()) :: subdirs

    override def toString: String =
      s"Directory(name=\"${name}\",files=${files.toString()},subdirs=${subdirs.toString()})"

    def treeToString(indent: Int = 0): String =
      val sb = mutable.StringBuilder()
      val indentStr = "  " * indent

      for subdir <- subdirs do
        sb.append(s"${indentStr}- ${subdir.name} (dir)\n")
        sb.append(subdir.treeToString(indent + 1))

      for file <- files do
        sb.append(s"${indentStr}- ${file.name} (file, size=${file.size})\n")

      sb.mkString

    def walkTree: List[FileNode] =
      val subdirWalks = subdirs.flatMap(_.walkTree)
      this :: files ++ subdirWalks

  def emptyRoot: FNode.Directory = FNode.Directory(None, "/", List(), List())


def processListing(currentDir: FNode.Directory, entries: List[ListingEntry]): Unit =
  for entry <- entries do
    entry match
      case ListingEntry.File(name, size) => currentDir.addFile(name, size)
      case ListingEntry.Directory(name) => currentDir.addSubdir(name)


def buildFileTree(commands: List[TerminalCommand]): FNode.Directory =
  var currentDir: FNode.Directory = FNode.emptyRoot

  for command <- commands do
    command match
      case TerminalCommand.ListCmd(entries) =>
        processListing(currentDir, entries)

      case TerminalCommand.ChangeDir("/") =>
        currentDir = currentDir.navigateToRoot

      case TerminalCommand.ChangeDir("..") =>
        currentDir = currentDir.navigateToParent

      case TerminalCommand.ChangeDir(dir) =>
        currentDir = currentDir.getSubdirByName(dir)
          .getOrElse(throw Exception(s"cd failed, no child dir ${dir} for ${currentDir.name}"))

  // Return the root
  currentDir.navigateToRoot


object Day7 extends SolutionWithParser[List[TerminalCommand], Int, Int]:
  override def dayNumber: Int = 7

  override def parser: Parser[List[TerminalCommand]] = Parsing.terminalOutputParser

  override def solvePart1(input: List[TerminalCommand]): Int =
    val tree = buildFileTree(input)
    tree
      .walkTree
      .filter(_.isDirectory)
      .map(_.size)
      .filter(_ < 100000)
      .sum

  override def solvePart2(input: List[TerminalCommand]): Int = ???


@main def run(): Unit = Day7.run()
@main def test(): Unit = Day7.test()
@main def testParser(): Unit =
  Day7.testParser(Parsing.terminalOutputParser)
@main def runParser(): Unit =
  Day7.runParser(Parsing.terminalOutputParser)