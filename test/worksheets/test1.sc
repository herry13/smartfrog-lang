import org.sf.lang._

object test1 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  val testDir = "/Users/admin/Documents/scala/smartfrog-lang/test"
                                                  //> testDir  : String = /Users/admin/Documents/scala/smartfrog-lang/test
  val file1 = testDir + "/test.sf"                //> file1  : String = /Users/admin/Documents/scala/smartfrog-lang/test/test.sf
  val s1 = Parser.parseFile(file1)                //> s1  : org.sf.lang.Store = (a,1)
  s1.normalize                                    //> res0: org.sf.lang.FlatStore = (a,1)

  val file2 = testDir + "/patrick1.sf"            //> file2  : String = /Users/admin/Documents/scala/smartfrog-lang/test/patrick1.
                                                  //| sf
  val s2 = Parser.parseFile(file2)                //> s2  : org.sf.lang.Store = (test,{(bar,1),(a1,{(foo,1)})}),(bar,2),(a2,{(foo,
                                                  //| 1)})
  s2.normalize                                    //> res1: org.sf.lang.FlatStore = (a2:foo,1),(a2,[Empty]),(bar,2),(test:a1:foo,1
                                                  //| ),(test:a1,[Empty]),(test:bar,1),(test,[Empty])
}