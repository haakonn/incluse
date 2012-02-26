package net.datapusher.incluse

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

@RunWith(classOf[JUnitRunner])
class PolicyMatchTest extends FunSuite {

  implicit def path(p: String) = p split("/") toList
  
  val pathAbcd = path("a/b/c/d")

  /**
   * Human-readable version of this policy (% is wildcard in this case):
   * <pre>
   * +/%%
   * -/%%/foo.txt
   * -/tmp/backup/%
   * +/tmp/backup/important
   * -/var/cache
   * </pre>
   */
  val samplePolicy = new Policy(List(
      Named("var",List(Named("cache",List(),Some(false))),None),
      RecWild(List(Named("foo.txt",List(),Some(false))),Some(true)),
      Named("tmp",List(Named("backup",List(Named("important",List(),Some(true)), Wild(List(),Some(false))),None)),None)))
  
  test("All-inclusive policy matches") {
    val allInclusive = new Policy(Seq(RecWild(accept=Some(true))))
    assert(allInclusive matches pathAbcd)
  }

  test("All-exclusive policy does not match") {
    val allExclusive = new Policy(Seq(RecWild(accept=Some(false))))
    assert(!(allExclusive matches pathAbcd))
  }

  test("Explicitly included despite neighbouring non-recursive exclusive wildcard") {
    assert(samplePolicy matches "tmp/backup/important")
  }

  test("Explicitly excluded") {
    assert(!(samplePolicy matches "var/cache"))
  }

  test("Implicitly included with straight match by recursive wildcard") {
    assert(samplePolicy matches pathAbcd)
  }

  test("Implicitly included with backtracking involved") {
    assert(samplePolicy matches "tmp/backup/other/stuff")
  }

  test("Implicitly excluded with straight match by non-recursive wildcard") {
    assert(!(samplePolicy matches "tmp/backup/nono"))
  }

  test("Explicitly excluded with straight match following recursive inclusive wildcard") {
    assert(!(samplePolicy matches "many/nodes/will/match/until/foo.txt"))
  }
  
}
