package net.datapusher.incluse

class Policy private (private val tree: Set[PolicyNode] = Set.empty) {

  import Policy._

  def matches(in: Seq[String]) = matchPolicy(tree, in).get

  def union(other: Policy) = new Policy(merge(tree, other.tree))
  
  private def visitAll[A](f: (PolicyNode, Seq[PolicyNode]) => A) =
    tree map(visit(_, Nil, f)) flatten

  override def toString = tree toString
  
  override def equals(that: Any) = that match {
    case p: Policy => tree == p.tree
    case _ => false
  }

  override def hashCode = tree.hashCode()

}

object Policy {
  
  def apply(tree: Set[PolicyNode] = Set.empty) = new Policy(tree)

  /** Matches a path against a policy.
   *  A return value of Some(true) means the path was included.
   *  Some(false) means it was excluded. A value of None will never be the returned,
   *  but is used internally to guide recursion.
   */
  private def matchPolicy(policy: Set[PolicyNode], in: Seq[String]): Option[Boolean] = {
    def matchClosest(policy: Set[PolicyNode], in: Seq[String], c: Option[PolicyNode]): Option[Boolean] = c match {
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
  
  private def findClosest(nodes: Set[PolicyNode], name: String): Option[PolicyNode] = {
    nodes.collectFirst { case x: Named if x.name == name => x }
    .orElse(nodes.collectFirst { case e: Wild => e })
    .orElse(nodes.collectFirst { case e: RecWild => e })
  }
  
  private def visit[A](node: PolicyNode, path: List[PolicyNode],
      f: ((PolicyNode, Seq[PolicyNode]) => A)): List[A] =
      f(node, path) :: node.children.foldLeft(List[A]())((acc, b) => acc ++ visit(b, node :: path, f))

  private def findSame(nodes: Traversable[PolicyNode], node: PolicyNode) = node match {
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
  
  /** Merges two policy trees. */
  def merge(n1: Set[PolicyNode], n2: Set[PolicyNode]): Set[PolicyNode] = {
    // Strategy:
    // for each in l:
    //   if same name exists in s, set l.children = merge(l.children, s.children)
    // for each in s:
    //   if same name does NOT exist in l, add s to l
    if (n1 isEmpty) { // if one of them is empty, pick the one that's not
      if (n2 isEmpty) Set.empty else n2
    } else if (n2 isEmpty) n1 else {
      // both are non-empty, an actual merge has to be done:
      val (l, s) = if (n1.size > n2.size) (n1, n2) else (n2, n1)
      val lMerged = l.map(lnode => findSame(s, lnode) match {
        case Some(n) => {
          val accept = or(lnode.accept, n.accept)
          val children = merge(lnode.children, n.children)
          lnode.cp(children, accept)
        }
        case None => lnode
      })
      normalize(lMerged union s.filterNot(findSame(lMerged, _).isDefined))
    }
  }

  /** Merges a PolicyNode into a node tree. */
  def merge(n: Set[PolicyNode], p: PolicyNode): Set[PolicyNode] = merge(n, Set(p))

  /** Remove redundancies, put into minimal form. */
  private def normalize(n: Set[PolicyNode]): Set[PolicyNode] = {
    // First extract polarities of any wildcards present at this level:
    val wildAccept = (n.collectFirst { case e: Wild => e.accept }).getOrElse(None)
    val recWildAccept = (n.collectFirst { case e: RecWild => e.accept }).getOrElse(None)
    val eitherWildAccept = recWildAccept.orElse(wildAccept.orElse(None))
    // Now use these to filter out superfluous nodes:
    val nf = n.filter(node =>
      if (node.accept.isDefined) {
        node match {
          case _: Named => node.accept != eitherWildAccept // Only keep if no wild excludes it
          case _: Wild => !recWildAccept.isDefined // RecWild > Wild regardless of polarities
          case _: RecWild => true // In fact, RecWild trumps everything
        }
      } else true
    )
    // You tolerated that, so now your children will be next:
    nf.map(x => x.cp(normalize(x.children)))
  }

}
