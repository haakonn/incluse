package net.datapusher.incluse

case class NodeSet(
    named: Set[Named] = Set.empty,
    wild: Option[Wild] = None,
    recWild: Option[RecWild] = None) {
  
  def -(node: PolicyNode[_]) = node match {
    case n: Named => copy(named - n)
    case _: Wild => copy(named, None, recWild)
    case _: RecWild => copy(named, wild, None)
  }

  def +(node: PolicyNode[_]) = node match {
    case n: Named => copy(named + n)
    case n: Wild => copy(wild = Some(n))
    case n: RecWild => copy(recWild = Some(n))
  }
  
  def isEmpty = this == NodeSet.empty
  
}

object NodeSet {
  val empty = NodeSet()
  def apply(node: PolicyNode[_]): NodeSet = empty + node
}
