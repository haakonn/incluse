package net.datapusher.incluse

import org.scalatest.FunSuite

class UnionTest extends FunSuite {

  private def cancelingPolarity(accept: Boolean, n1: Policy, n2: Policy) = {
    assertResult(n1) { n1 union n2 }
    assertResult(n1) { n2 union n1 }
  }

  private def simpleCancelingPolarity(accept: Boolean, comp: Policy) =
    cancelingPolarity(accept, comp, Policy(NodeSet(Set(Named("a", accept=Some(accept))))))

  private def wildEliminatesNamed(polarity: Boolean) =
    simpleCancelingPolarity(polarity, Policy(NodeSet(wild=Some(Wild(accept=Some(polarity))))))

  private def eliminationByRecWild(recWild: Policy, other: Policy) =
    assertResult(recWild) { recWild union other }

  test("Union is symmetrical") {
    assertResult(Policies.samplePolicy union Policies.smallPolicy) {
      Policies.smallPolicy union Policies.samplePolicy
    }
  }

  def selfUnion(p: Policy) = assertResult(p) { p union p }

  test("Simple self-union") {
    selfUnion(Policies.smallPolicy)
  }

  test("Complex self-union") {
    selfUnion(Policies.samplePolicy)
  }

  def unionWithEmpty(p: Policy) = assertResult(p) { p union Policy.empty }

  test("Simple union with empty policy") {
    unionWithEmpty(Policies.smallPolicy)
  }

  test("Complex union with empty policy") {
    unionWithEmpty(Policies.samplePolicy)
  }

  test("Union of two unequals") {
    assertResult(Policy(NodeSet(Set(Named("a", accept=Some(true)), Named("b", accept=Some(true)))))) {
      Policies.smallPolicy union Policies.smallPolicy2
    }
  }

  test("Wild eliminates named on positive polarity") {
    wildEliminatesNamed(true)
  }

  test("Wild eliminates named on negative polarity") {
    wildEliminatesNamed(false)
  }

  test("RecWild explicitly eliminates Wild on positive polarity") {
    eliminationByRecWild(Policies.allInclusive, Policies.allInclNonRec)
  }

  test("RecWild explicitly eliminates Wild on negative polarity") {
    eliminationByRecWild(Policies.allExclusive, Policies.allExclNonRec)
  }

  test("RecWild explicitly eliminates Wild on opposing polarities") {
    eliminationByRecWild(Policies.allInclusive, Policies.allExclNonRec)
  }

  test("RecWild explicitly eliminates named on positive polarity") {
    simpleCancelingPolarity(true, Policies.allInclusive)
  }

  test("RecWild explicitly eliminates named on negative polarity") {
    simpleCancelingPolarity(false, Policies.allExclusive)
  }

  private def implicitElimination(accept: Boolean) = {
    val p = Policy(NodeSet(Set(Named("a", children=NodeSet(Set(Named("b", accept=Some(accept))))))))
    cancelingPolarity(accept, if (accept) Policies.allInclusive else Policies.allExclusive, p)
  }

  test("RecWild implicitly eliminates named on positive polarity") {
    implicitElimination(true)
  }

  test("RecWild implicitly eliminates named on negative polarity") {
    implicitElimination(false)
  }

  test("Polar RecWild does not explicitly eliminate non-polar Wild") {
    // p = +a/*/b
    val p = Policy(NodeSet(Set(Named("a",NodeSet(Set(),Some(Wild(NodeSet(Set(Named("b",NodeSet(Set(),None,None),Some(true))),None,None),None)),None),None)),None,None))
    // p2 = -a/**
    val p2 = Policy(NodeSet(Set(Named("a",NodeSet(Set(),None,Some(RecWild(NodeSet(Set(),None,None),Some(false)))),None)),None,None))
    // expected = p, p2
    val expected = Policy(NodeSet(Set(Named("a",NodeSet(Set(),Some(Wild(NodeSet(Set(Named("b",NodeSet(Set(),None,None),Some(true))),None,None),None)),Some(RecWild(NodeSet(Set(),None,None),Some(false)))),None)),None,None))
    assertResult(expected) {
      p union p2
    }
  }

}
