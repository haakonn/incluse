package net.datapusher.incluse

object Policies {
  val allInclusive = Policy(Set(RecWild(accept=Some(true))))
  val allExclusive = Policy(Set(RecWild(accept=Some(false))))
  
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