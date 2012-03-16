package net.datapusher.incluse

class Policy private (private val tree: NodeSet) {

  import Policy._

  def matches(in: Seq[String]) = matchPolicy(tree, in).get

  def union(other: Policy) = new Policy(normalize(merge(tree, other.tree)))
  
  override def toString = tree toString
  
  override def equals(that: Any) = that match {
    case p: Policy => tree == p.tree
    case _ => false
  }

  override val hashCode = tree.hashCode()

}

object Policy {

  def apply(tree: NodeSet) = new Policy(tree)

  val empty = new Policy(NodeSet.empty)

  /** Matches a path against a policy.
   *  A return value of Some(true) means the path was included.
   *  Some(false) means it was excluded. A value of None will never be the returned,
   *  but is used internally to guide recursion.
   */
  private def matchPolicy(policy: NodeSet, in: Seq[String]): Option[Boolean] = {
    def matchClosest(policy: NodeSet, in: Seq[String], c: Option[PolicyNode[_]]): Option[Boolean] = c match {
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
                  case None => matchPolicy(policy - closest, in) // backtrack
                  case b => b
                }
            }
          case some => some
        }
      case None => None
    }
    matchClosest(policy, in, findClosest(policy, in.head))
  }

  private def accept(node: PolicyNode[_], in: Seq[String]): Option[Boolean] = {
    if (in.length == 1) {
      node match {
        case Named(name, _, a) if name == in.head => a
        case Named(_, _, _) => Some(false)
        case Wild(_, a) => a
        case RecWild(_, a) => a
      }
    } else {
      None // Continue matching
    }
  }

  private def findNamed(nodes: Set[Named], name: String): Option[Named] =
    nodes.collectFirst { case n @ Named(nname, _, _) if name == nname => n }
  
  private def findClosest(nodes: NodeSet, name: String): Option[PolicyNode[_]] = {
    val named: Option[PolicyNode[_]] = findNamed(nodes.named, name)
    named orElse nodes.wild orElse nodes.recWild
  }

  private def findSame(nodes: Set[Named], node: Named) = findNamed(nodes, node.name)

  private def or(a: Option[Boolean], b: Option[Boolean]) = (a, b) match {
    case (None, None) => None
    case (Some(_), None) => a
    case (None, Some(_)) => b
    case (Some(ab), Some(bb)) => Some(ab || bb) // true wins over false when conflict
  }
  
  /** Merges two policy trees. */
  def merge(n1: NodeSet, n2: NodeSet): NodeSet =
    // Strategy:
    // for each in l:
    //   if same name exists in s, set l.children = merge(l.children, s.children)
    // for each in s:
    //   if same name does NOT exist in l, add s to l
    if (n1 isEmpty) n2 // if one of them is empty, pick the one that's not
    else if (n2 isEmpty) n1 else {
      // both are non-empty, an actual merge has to be done:
      val named = merge(n1.named, n2.named)
      val wild = merge(n1.wild, n2.wild)
      val recWild = merge(n1.recWild, n2.recWild)
      NodeSet(named, wild, recWild)
    }

  private def merge(n1: Set[Named], n2: Set[Named]): Set[Named] = {
    val (l, s) = if (n1.size > n2.size) (n1, n2) else (n2, n1)
    val lMerged = l.map(lnode => findSame(s, lnode) match {
      case Some(n) => {
        val accept = or(lnode.accept, n.accept)
        val children = merge(lnode.children, n.children)
        lnode.cp(children, accept)
      }
      case None => lnode
    })
    lMerged union s.filterNot(findSame(lMerged, _).isDefined)
  }
  
  private def merge[A <: PolicyNode[A]](n1: Option[A], n2: Option[A]) : Option[A] = {
    if (n1.isDefined) {
      if (n2.isDefined) {
        val (w1, w2) = (n1.get, n2.get)
        val accept = or(w1.accept, w2.accept)
        Some(w1.cp(merge(w1.children, w2.children), accept))
      } else n1
    } else n2
  }
  
  /** Merges a PolicyNode into a node set. */
  def merge(n: NodeSet, p: PolicyNode[_]): NodeSet = merge(n, NodeSet(p))

  /** Remove redundancies, put into minimal form. */
  private def normalize(n: NodeSet, accept: Option[Boolean] = None): NodeSet = {
    // First extract polarities of any wildcards present:
    val wildAccept = n.wild.flatMap { _.accept }
    val recAccept = n.recWild.flatMap { _.accept }
    val inheritAccept = recAccept orElse accept
    val eitherAccept = wildAccept orElse inheritAccept
    // Now use these to filter out superfluous nodes.
    def r[A <: PolicyNode[A]](n: A, incl: Boolean) =
      if (incl) {
        val rn = n.cp(normalize(n.children, inheritAccept))
        if (!rn.accept.isDefined && rn.children.isEmpty) None else Some(rn)
      } else None
    val rNamed = n.named.flatMap(x => r(x, (!x.accept.isDefined || x.accept != eitherAccept)))
    val rWild = n.wild.flatMap(w =>
      r(w, !w.accept.isDefined || ((accept.isDefined && accept != w.accept) || !recAccept.isDefined)))
    val rRecWild = n.recWild.flatMap(x => r(x, true))
    NodeSet(rNamed, rWild, rRecWild)
  }

}
