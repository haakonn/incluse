package net.datapusher.incluse
package parsers

import scala.io.Source
import net.datapusher.incluse._

/**
 * A parser for file system policies. The format is a set of
 * newline-separated lines that starts with either + or -,
 * immediately followed by the path in question. A * will match
 * any single directory, while ** will match recursively.
 */
class FilePolicyParser extends PolicyParser {
  
  def parse(in: Source) = {
    val lines = in.getLines
    val tree = lines.foldLeft(Seq[PolicyNode]())((acc, line) => Policy.merge(acc, parseLine(line)))
    new Policy(tree)
  }
  
  def parseLine(line: String): PolicyNode = {
    val parts = line.substring(2).split("/")
    val accept = Some(line(0) == '+')
    parts.foldRight(None: Option[PolicyNode])((value, acc) => {
      val node = parseNode(value)
      acc match {
        case Some(child) => Some(node.cp(Seq(child), None))
        case None => Some(node.cp(Nil, accept))
      }
    }).get
  }

  def parseNode(value: String): PolicyNode = value match {
    case "*" => Wild()
    case "**" => RecWild()
    case name => Named(name)
  }

}
