package net.datapusher.incluse

import org.scalatest.FunSuite

class NodeSetTest extends FunSuite {
  
  test("Empty set isEmpty") {
    assert(NodeSet.empty isEmpty)
  }
  
}