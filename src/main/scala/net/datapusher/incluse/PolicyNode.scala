package net.datapusher.incluse

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
  override val hashCode = name.hashCode() + super.hashCode
}
