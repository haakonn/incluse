package net.datapusher.incluse

import org.scalatest.FunSuite

class PolicyNodeTest extends FunSuite {

  private def assertUnequal[A <: PolicyNode[A], B <: PolicyNode[B]](n1: PolicyNode[A], n2: PolicyNode[B]) = {
    assert(!(n1 == n2))
    assert(!(n2 == n1))
  }

  private def assertEqual[A <: PolicyNode[A], B <: PolicyNode[B]](n1: PolicyNode[A], n2: PolicyNode[B]) = {
    assert(n1 == n2)
    assert(n2 == n1)
  }

  test("Simple equalities") {
    assertEqual(Wild(), Wild())
    assertEqual(RecWild(), RecWild())
    assertEqual(Named("a"), Named("a"))
  }

  test("Simple inequalities") {
    assertUnequal(Wild(), RecWild())
    assertUnequal(Named("a"), RecWild())
    assertUnequal(Named("a"), Wild())
  }

  test("Wild not equal when polarities oppose") {
    assertUnequal(Wild(accept=Some(true)), Wild(accept=Some(false)))
  }

  test("Wild not equal when children differ") {
    assertUnequal(Wild(), Wild(NodeSet(Set(Named("a")))))
  }

  test("Wild equal when children equal") {
    assertEqual(Wild(NodeSet(Set(Named("a")))), Wild(NodeSet(Set(Named("a")))))
  }
  
  test("Named equal when names and children are equal") {
    assertEqual(Named("a", NodeSet(wild=Some(Wild()))), Named("a", NodeSet(wild=Some(Wild()))))
  }
  
  test("Named unequal when names and children are equal but polarities oppose") {
    assertUnequal(Named("a", NodeSet(wild=Some(Wild(accept=Some(true))))), Named("a", NodeSet(wild=Some(Wild(accept=Some(false))))))
  }

  test("Named unequal when names and children are equal one node is nonpolar") {
    assertUnequal(Named("a", NodeSet(wild=Some(Wild(accept=Some(true))))), Named("a", NodeSet(wild=Some(Wild()))))
  }

  test("Named equal when names, children and polarities are the same") {
    assertEqual(Named("a", NodeSet(wild=Some(Wild(accept=Some(true))))), Named("a", NodeSet(wild=Some(Wild(accept=Some(true))))))
  }
  
}
