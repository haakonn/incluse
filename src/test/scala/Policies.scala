package net.datapusher.incluse

object Policies {
  val allInclusive = Policy(Seq(RecWild(accept=Some(true))))
  val allExclusive = Policy(Seq(RecWild(accept=Some(false))))
  
  /** Human-readable version of this policy (% is wildcard in this case):
    * <pre>
    * +/%%
    * -/%%/foo.txt
    * -/tmp/backup/%
    * +/tmp/backup/important
    * -/var/cache
    * </pre>
    */
  val samplePolicy = Policy(List(
      Named("var",List(Named("cache",List(),Some(false))),None),
      RecWild(List(Named("foo.txt",List(),Some(false))),Some(true)),
      Named("tmp",List(Named("backup",List(Named("important",List(),Some(true)), Wild(List(),Some(false))),None)),None)))

}