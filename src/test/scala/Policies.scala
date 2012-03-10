package net.datapusher.incluse

object Policies {
  val allInclusive = Policy(NodeSet(recWild=Some(RecWild(accept=Some(true)))))
  val allExclusive = Policy(NodeSet(recWild=Some(RecWild(accept=Some(false)))))
  
  val allInclNonRec = Policy(NodeSet(wild=Some(Wild(accept=Some(true)))))
  val allExclNonRec = Policy(NodeSet(wild=Some(Wild(accept=Some(false)))))
  
  val smallPolicy = Policy(NodeSet(Set(Named("a", accept=Some(true)))))
  val smallPolicy2 = Policy(NodeSet(Set(Named("b", accept=Some(true)))))

  
  /** Human-readable version of this policy (% is wildcard in this case):
    * <pre>
    * +/%%
    * -/%%/foo.txt
    * -/tmp/backup/%
    * +/tmp/backup/important
    * -/var/cache
    * </pre>
    */
  val samplePolicy = Policy(NodeSet(
      Set(Named("var", NodeSet(Set(Named("cache",NodeSet(Set(),None,None),Some(false))),None,None),None),
          Named("tmp", NodeSet(Set(Named("backup",NodeSet(Set(Named("important",NodeSet(Set(),None,None),Some(true))),Some(Wild(NodeSet(Set(),None,None),Some(false))),None),None)),None,None),None)),None,
          Some(RecWild(NodeSet(Set(Named("foo.txt",NodeSet(Set(),None,None),Some(false))),None,None),Some(true)))))
  
}
