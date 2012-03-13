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
  
  def isEmpty = named.isEmpty && wild == None && recWild == None
  
}

object NodeSet {
  val empty = NodeSet()
  def apply(node: PolicyNode[_]): NodeSet = empty + node
}

abstract sealed class PolicyNode[V <: PolicyNode[V]](
    val children: NodeSet,
    val accept: Option[Boolean]) {
  def cp(children: NodeSet, accept: Option[Boolean] = accept): V
  override def equals(that: Any) = that match {
    case node: PolicyNode[_] =>
      getClass == node.getClass &&
      accept == node.accept &&
      children == node.children
    case _ => false
  }
  override def hashCode = getClass.hashCode() + accept.hashCode() + children.hashCode()
}

case class Wild(
    override val children: NodeSet = NodeSet.empty,
    override val accept: Option[Boolean] = None)
    extends PolicyNode[Wild](children, accept) {
  override def cp(c: NodeSet, accept: Option[Boolean] = accept) = copy(c, accept)
}

case class RecWild(
    override val children: NodeSet = NodeSet.empty,
    override val accept: Option[Boolean] = None)
    extends PolicyNode[RecWild](children, accept) {
  override def cp(c: NodeSet, accept: Option[Boolean] = accept) = copy(c, accept)
}

case class Named(
    val name: String,
    override val children: NodeSet = NodeSet.empty,
    override val accept: Option[Boolean] = None)
    extends PolicyNode[Named](children, accept) {
  override def cp(c: NodeSet, accept: Option[Boolean] = accept) = copy(name, c, accept)
  override def equals(that: Any) = that match {
    case node: Named => name == node.name && super.equals(that)
    case _ => false
  }
  override def hashCode = name.hashCode() + super.hashCode()
}
