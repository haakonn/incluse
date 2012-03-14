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
    val tree = lines.foldLeft(NodeSet.empty)((set, line) => Policy.merge(set, parseLine(line)))
    Policy(tree)
  }
  
  def parseLine(line: String): PolicyNode[_] = {
    val parts = line.substring(2).split('/')
    val accept = Some(line(0) == '+')
    parts.foldRight(None: Option[PolicyNode[_]])((value, acc) => {
      val node = parseNode(value)
      acc match {
        case Some(child) => Some(node.cp(node.children + child))
        case None => Some(node.cp(NodeSet.empty, accept))
      }
    }).get
  }

  def parseNode(value: String) = value match {
    case "*" => Wild()
    case "**" => RecWild()
    case name => Named(name)
  }

}
