import org.sf.lang._

object test1 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  val r1 = Reference(List("a", "THIS", "b", "c", "THIS"))
                                                  //> r1  : org.sf.lang.Reference = a:THIS:b:c:THIS
  Reference.empty.trace(r1)                       //> res0: org.sf.lang.Reference = a:b:c
  val r2 = Reference(List("a", "THIS", "b", "ROOT", "c", "THIS"))
                                                  //> r2  : org.sf.lang.Reference = a:THIS:b:ROOT:c:THIS
  Reference.empty.trace(r2)                       //> res1: org.sf.lang.Reference = c
  val r3 = Reference(List("a", "THIS", "b", "ROOT", "c", "d", "PARENT", "THIS", "x"))
                                                  //> r3  : org.sf.lang.Reference = a:THIS:b:ROOT:c:d:PARENT:THIS:x
  Reference.empty.trace(r3)                       //> res2: org.sf.lang.Reference = d:x

  Reference("a").trace(Reference(List("THIS","PARENT")))
                                                  //> res3: org.sf.lang.Reference = [empty]

  /*val testDir = "/Users/admin/Documents/scala/smartfrog-lang/test"
  val file1 = testDir + "/test.sf"
  val s1 = Parser.parseFile(file1)
  s1.normalize

  val file2 = testDir + "/patrick1.sf"
  val s2 = Parser.parseFile(file2)
  s2.normalize
    */
}