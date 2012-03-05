package net.datapusher.incluse

import org.scalatest.FunSuite

class UnionTest extends FunSuite {

  private def simpleCancelingPolarity(accept: Boolean, comp: Policy) = {
    val p = Policy(Set(Named("a", accept=Some(accept))))
    assert((p union comp) === comp)
    assert((comp union p) === comp)
  }

  def wildEliminatesNamed(polarity: Boolean) =
    simpleCancelingPolarity(polarity, Policy(Set(Wild(accept=Some(polarity)))))
  
  def eliminationByRecWild(recWild: Policy, other: Policy) =
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

  test("Union of two unequals") {
    val union = Policies.smallPolicy union Policies.smallPolicy2
    val expected = Policy(Set(Named("a", accept=Some(true)), Named("b", accept=Some(true))))
    assert(union === expected)
  }
  
  test("Wild eliminates named on positive polarity") {
    wildEliminatesNamed(true)
  }
  
  test("Wild eliminates named on negative polarity") {
    wildEliminatesNamed(false)
  }

  test("RecWild eliminates Wild on positive polarity") {
    eliminationByRecWild(Policies.allInclusive, Policies.allInclNonRec)
  }

  test("RecWild eliminates Wild on negative polarity") {
    eliminationByRecWild(Policies.allExclusive, Policies.allExclNonRec)
  }

  test("RecWild eliminates Wild on opposing polarities") {
    eliminationByRecWild(Policies.allInclusive, Policies.allExclNonRec)
  }

  test("RecWild eliminates named on positive polarity") {
    simpleCancelingPolarity(true, Policies.allInclusive)
  }

  test("RecWild eliminates named on negative polarity") {
    simpleCancelingPolarity(false, Policies.allExclusive)
  }

}
