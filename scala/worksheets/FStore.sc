import org.sf.lang._

object FStore {
  import Domain._

  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val s = put(Empty, "x", 1)                      //> s  : String => Any = <function1>
  s("x")                                          //> res0: Any = 1
  put(s, "x", 2)("x")                             //> res1: Any = 2
  
  val r1 = Reference("a")                         //> r1  : org.sf.lang.Reference = a
  val s1 = bind(Empty, r1, 2)                     //> s1  : String => Any = <function1>
  s1("a")                                         //> res2: Any = 2
  s1("c")                                         //> res3: Any = [undefined]
  val s2 = bind(s1, r1, Empty)                    //> s2  : String => Any = <function1>
  s2("a")                                         //> res4: Any = <function1>
  val r3 = Reference(List("a", "b"))              //> r3  : org.sf.lang.Reference = a:b
  val s3 = bind(s2, r3, 3)                        //> s3  : String => Any = <function1>
  s3("a")                                         //> res5: Any = <function1>
  find(s1, r3)                                    //> res6: Any = [undefined]
  find(s2, r3)                                    //> res7: Any = [undefined]
  find(s3, r3)                                    //> res8: Any = 3
  
  val r4 = Reference("c")                         //> r4  : org.sf.lang.Reference = c
  val s4 = bind(s3, r4, Empty)                    //> s4  : String => Any = <function1>
  s4("c")                                         //> res9: Any = <function1>
  s4("a")                                         //> res10: Any = <function1>
  find(s4, r1)                                    //> res11: Any = <function1>
  find(s4, r3)                                    //> res12: Any = 3
  val s5 = inherit(s4, Reference.Empty, r1, r4)   //> s5  : String => Any = <function1>
  s5("c")                                         //> res13: Any = <function1>
  find(s5, r1)                                    //> res14: Any = <function1>
  find(s5, r3)                                    //> res15: Any = 3
  val r5 = Reference(List("c", "b"))              //> r5  : org.sf.lang.Reference = c:b
  find(s5, r5)                                    //> res16: Any = 3
  find(s5, Reference(List("a", "c")))             //> res17: Any = [undefined]
  find(s5, Reference(List("c", "a")))             //> res18: Any = [undefined]
  
  /*
  val s3 = Domain.bind(s2, r1, Domain.Empty)
  s3("a")
  val r2 = Reference(List("a", "b"))
  //Domain.bind(s2, r2, 3)
  val s4 = Domain.bind(s3, r2, 3)
  s4("b")
  find(s4, r1)
  find(s4, r2)
  */
}