package net.datapusher.incluse

import scala.language.implicitConversions
import org.scalatest.FunSuite

class MatchTest extends FunSuite {

  implicit def path(p: String) = p.split("/").toList

  val pathAbcd = path("a/b/c/d")

  test("All-inclusive policy matches") {
    assert(Policies.allInclusive matches pathAbcd)
  }

  test("All-exclusive policy does not match") {
    assert(!(Policies.allExclusive matches pathAbcd))
  }

  test("Explicitly included despite neighbouring non-recursive exclusive wildcard") {
    assert(Policies.samplePolicy matches "tmp/backup/important")
  }

  test("Explicitly excluded") {
    assert(!(Policies.samplePolicy matches "var/cache"))
  }

  test("Implicitly included with straight match by recursive wildcard") {
    assert(Policies.samplePolicy matches pathAbcd)
  }

  test("Implicitly included with backtracking involved") {
    assert(Policies.samplePolicy matches "tmp/backup/other/stuff")
  }

  test("Implicitly excluded with straight match by non-recursive wildcard") {
    assert(!(Policies.samplePolicy matches "tmp/backup/nono"))
  }

  test("Explicitly excluded with straight match following recursive inclusive wildcard") {
    assert(!(Policies.samplePolicy matches "many/nodes/will/match/until/foo.txt"))
  }
  
}
