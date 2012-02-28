package net.datapusher.incluse

class Policy(tree: Seq[PolicyNode] = Nil) {

  import Policy._

  def matches(in: Seq[String]) = matchPolicy(tree, in).getOrElse(false)

  def union(other: Seq[PolicyNode]) = merge(tree, other)
  
  private def visitAll[A](f: (PolicyNode, Seq[PolicyNode]) => A) =
    tree map(visit(_, Nil, f)) flatten

  override def toString = tree toString

}

object Policy {

  /** Matches a path against a policy.
    * A return value of Some(true) means the path was included.
    * Some(false) means it was excluded. A value of None will never be the returned,
    * but is used internally to guide recursion.
    */
  private def matchPolicy(policy: Seq[PolicyNode], in: Seq[String]): Option[Boolean] = {
    def matchClosest(policy: Seq[PolicyNode], in: Seq[String], c: Option[PolicyNode]): Option[Boolean] = c match {
      case Some(closest) =>
        val tail = in.tail
        accept(closest, in) match {
          case None =>
            closest match {
              case _: RecWild =>
                findClosest(closest.children, tail.head) match { // peak ahead
                  case None => matchClosest(policy, tail, c) // repeat RecWild match
                  case x => matchClosest(closest.children, tail, x) // something matching next step, stop RecWild
                }
              case _ =>
                matchPolicy(closest.children, tail) match {
                  case None => matchPolicy(policy filterNot (_ == closest), in) // backtrack
                  case b => b
                }
            }
          case some => some
        }
      case None => None
    }
    matchClosest(policy, in, findClosest(policy, in.head))
  }

  private def accept(node: PolicyNode, in: Seq[String]): Option[Boolean] = {
    if (in.length == 1) {
      node match {
        case Named(name, _, a) if name == in.head => a
        case Named(name, _, a) => Some(false)
        case Wild(_, a) => a
        case RecWild(_, a) => a
      }
    } else {
      None // Continue matching
    }
  }
  
  private def findClosest(nodes: Seq[PolicyNode], name: String): Option[PolicyNode] = {
    nodes.collectFirst { case x: Named if x.name == name => x }
    .orElse(nodes.collectFirst { case e: Wild => e })
    .orElse(nodes.collectFirst { case e: RecWild => e })
  }
  
  private def visit[A](node: PolicyNode, path: List[PolicyNode],
      f: ((PolicyNode, Seq[PolicyNode]) => A)): List[A] =
      f(node, path) :: node.children.foldLeft(List[A]())((acc, b) => acc ++ visit(b, node :: path, f))

  private def findSame(nodes: Seq[PolicyNode], node: PolicyNode) = node match {
    case Named(name, _, _) => { nodes find { case Named(nname, _, _) => name == nname; case _ => false } }
    case _: Wild => { nodes find { case _: Wild => true; case _ => false } }
    case _: RecWild => { nodes find { case _: RecWild => true; case _ => false } }
  }
  
  private def or(a: Option[Boolean], b: Option[Boolean]) = (a, b) match {
    case (None, None) => None
    case (Some(_), None) => a
    case (None, Some(_)) => b
    case (Some(ab), Some(bb)) => Some(ab || bb) // true wins over false when conflict
  }

  /**
   * Merges two policy trees.
   */
  def merge(n1: Seq[PolicyNode], n2: Seq[PolicyNode]): Seq[PolicyNode] = (n1, n2) match {
    // Strategy:
    // for each in l:
    //   if same name exists in s, set l.children = merge(l.children, s.children)
    // for each in s:
    //   if same name does NOT exist in l, add s to l
    case (Nil, Nil) => Nil
    case (x, Nil) => x
    case (Nil, x) => x
    case (x, y) => {
      val (l, s) = if (x.length > y.length) (x, y) else (y, x)
      val lMerged = l.map(lnode => findSame(s, lnode) match {
        case Some(n) => {
          val accept = or(lnode.accept, n.accept)
          val children = merge(lnode.children, n.children)
          lnode.cp(children, accept)
        }
        case None => lnode
      })
      s.foldLeft(lMerged)((acc, n) => if (findSame(acc, n).isDefined) acc else n +: acc)
    }
  }
  
  /**
   * Merges a PolicyNode into a node tree.
   */
  def merge(n: Seq[PolicyNode], p: PolicyNode): Seq[PolicyNode] = merge(n, Seq(p))
      
}
