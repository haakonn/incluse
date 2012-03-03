package net.datapusher.incluse

abstract sealed class PolicyNode(
    val children: Seq[PolicyNode],
    val accept: Option[Boolean]) {
  def cp(children: Seq[PolicyNode], accept: Option[Boolean]): PolicyNode
  override def equals(that: Any) = that match {
    case node: PolicyNode => accept == node.accept && children.toSet == node.children.toSet
    case _ => false
  }
  override def hashCode = accept.hashCode() + children.hashCode()
}

case class Wild(
    override val children: Seq[PolicyNode] = List(),
    override val accept: Option[Boolean] = None)
    extends PolicyNode(children, accept) {
  override def cp(c: Seq[PolicyNode], accept: Option[Boolean]) = copy(c, accept)
}

case class RecWild(
    override val children: Seq[PolicyNode] = List(),
    override val accept: Option[Boolean] = None)
    extends PolicyNode(children, accept) {
  override def cp(c: Seq[PolicyNode], accept: Option[Boolean]) = copy(c, accept)
}

case class Named(
    val name: String,
    override val children: Seq[PolicyNode] = List(),
    override val accept: Option[Boolean] = None)
    extends PolicyNode(children, accept) {
  override def cp(c: Seq[PolicyNode], accept: Option[Boolean]) = copy(name, c, accept)
  override def equals(that: Any) = that match {
    case node: Named => name == node.name && super.equals(that)
    case _ => false
  }
  override def hashCode() = name.hashCode() + super.hashCode()
}
