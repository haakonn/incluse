package net.datapusher.incluse

object Policies {
  val allInclusive = Policy(Set(RecWild(accept=Some(true))))
  val allExclusive = Policy(Set(RecWild(accept=Some(false))))
  
  val allInclNonRec = Policy(Set(Wild(accept=Some(true))))
  val allExclNonRec = Policy(Set(Wild(accept=Some(false))))
  
  val smallPolicy = Policy(Set(Named("a", accept=Some(true))))
  val smallPolicy2 = Policy(Set(Named("b", accept=Some(true))))

  
  /** Human-readable version of this policy (% is wildcard in this case):
    * <pre>
    * +/%%
    * -/%%/foo.txt
    * -/tmp/backup/%
    * +/tmp/backup/important
    * -/var/cache
    * </pre>
    */
  val samplePolicy = Policy(Set(
      Named("var",Set(Named("cache",Set.empty,Some(false))),None),
      RecWild(Set(Named("foo.txt",Set.empty,Some(false))),Some(true)),
      Named("tmp",Set(Named("backup",Set(Named("important",Set(),Some(true)), Wild(Set.empty,Some(false))),None)),None)))

}