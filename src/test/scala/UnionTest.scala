package net.datapusher.incluse

import org.scalatest.FunSuite

class UnionTest extends FunSuite {

  private def cancelingPolarity(accept: Boolean, n1: Policy, n2: Policy) = {
    assert((n1 union n2) === n1)
    assert((n2 union n1) === n1)
  }

  private def simpleCancelingPolarity(accept: Boolean, comp: Policy) =
    cancelingPolarity(accept, comp, Policy(NodeSet(Set(Named("a", accept=Some(accept))))))

  private def wildEliminatesNamed(polarity: Boolean) =
    simpleCancelingPolarity(polarity, Policy(NodeSet(wild=Some(Wild(accept=Some(polarity))))))

  private def eliminationByRecWild(recWild: Policy, other: Policy) =
    assert((recWild union other) === recWild)    

  test("Union is symmetrical") {
    val p1 = Policies.samplePolicy union Policies.smallPolicy
    val p2 = Policies.smallPolicy union Policies.samplePolicy
    assert(p1 === p2)
  }

  test("Self-union") {
    val policy = Policies.samplePolicy
    val self = policy union policy
    assert(self === policy)
  }

  test("Union with empty policy") {
    assert((Policies.samplePolicy union Policy()) === Policies.samplePolicy)
  }

  test("Union of two unequals") {
    val union = Policies.smallPolicy union Policies.smallPolicy2
    val expected = Policy(NodeSet(Set(Named("a", accept=Some(true)), Named("b", accept=Some(true)))))
    assert(union === expected)
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

}
