package net.datapusher.incluse

abstract sealed class PolicyNode(
    val children: Set[PolicyNode],
    val accept: Option[Boolean]) {
  def cp(children: Set[PolicyNode], accept: Option[Boolean]): PolicyNode
  override def equals(that: Any) = that match {
    case node: PolicyNode =>
      getClass == node.getClass &&
      accept == node.accept &&
      children == node.children
    case _ => false
  }
  override def hashCode = getClass.hashCode() + accept.hashCode() + children.hashCode()
}

case class Wild(
    override val children: Set[PolicyNode] = Set.empty,
    override val accept: Option[Boolean] = None)
    extends PolicyNode(children, accept) {
  override def cp(c: Set[PolicyNode], accept: Option[Boolean]) = copy(c, accept)
}

case class RecWild(
    override val children: Set[PolicyNode] = Set.empty,
    override val accept: Option[Boolean] = None)
    extends PolicyNode(children, accept) {
  override def cp(c: Set[PolicyNode], accept: Option[Boolean]) = copy(c, accept)
}

case class Named(
    val name: String,
    override val children: Set[PolicyNode] = Set.empty,
    override val accept: Option[Boolean] = None)
    extends PolicyNode(children, accept) {
  override def cp(c: Set[PolicyNode], accept: Option[Boolean]) = copy(name, c, accept)
  override def equals(that: Any) = that match {
    case node: Named => name == node.name && super.equals(that)
    case _ => false
  }
  override def hashCode = name.hashCode() + super.hashCode()
}
