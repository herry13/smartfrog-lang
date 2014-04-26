import org.sf.lang._

object test1 {
  //println("Welcome to the Scala worksheet")

  val testDir = "/Users/admin/Documents/scala/smartfrog-lang/test"
                                                  //> testDir  : String = /Users/admin/Documents/scala/smartfrog-lang/test
  /*
  val file1 = testDir + "/test.sf"
  val s1 = Parser.parseFile(file1)
     
  val file2 = testDir + "/herry4.sf"
  val s2 = Parser.parseFile(file2)
  //s2.delete("c")
  val r = Reference(List("b", "c"))
  s2.find(r)
  s2.bindPivot(r, 4, "b", Store.putBefore)
  */

  val file3 = testDir + "/order1.sf"              //> file3  : String = /Users/admin/Documents/scala/smartfrog-lang/test/order1.sf
                                                  //| 
  val s3 = Parser.parseFile(file3)                //> s3  : org.sf.lang.Store = (c,4),(a,5),(b,2)
}