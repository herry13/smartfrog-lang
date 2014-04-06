import org.sf.lang._

object FStore {
  import Domain._

  Empty                                           //> res0: String => Any = [empty]

  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val s = put(Empty, "x", 1)                      //> s  : String => Any = (x,1),[empty]
  s("x")                                          //> res1: Any = 1
  put(s, "x", 2)("x")                             //> res2: Any = 2
 
  val r1 = List("a")                              //> r1  : List[String] = List(a)
  val s1 = bind(Empty, r1, 2)                     //> s1  : String => Any = (a,2),[empty]
  s1("a")                                         //> res3: Any = 2
  s1("c")                                         //> res4: Any = [undefined]
  val s2 = bind(s1, r1, Empty)                    //> s2  : String => Any = (a,[empty]),(a,2),[empty]
  s2("a")                                         //> res5: Any = [empty]
  val r3 = List("a", "b")                         //> r3  : List[String] = List(a, b)
  val s3 = bind(s2, r3, 3)                        //> s3  : String => Any = (a,(b,3),[empty]),(a,[empty]),(a,2),[empty]
  s3("a")                                         //> res6: Any = (b,3),[empty]
  find(s1, r3)                                    //> res7: Any = [undefined]
  find(s2, r3)                                    //> res8: Any = [undefined]
  find(s3, r3)                                    //> res9: Any = 3
    
  val r4 = List("c")                              //> r4  : List[String] = List(c)
  val s4 = bind(s3, r4, Empty)                    //> s4  : String => Any = (c,[empty]),(a,(b,3),[empty]),(a,[empty]),(a,2),[empty
                                                  //| ]
  s4("c")                                         //> res10: Any = [empty]
  s4("a")                                         //> res11: Any = (b,3),[empty]
  find(s4, r1)                                    //> res12: Any = (b,3),[empty]
  find(s4, r3)                                    //> res13: Any = 3
  val s5 = inherit(s4, List(), r1, r4)            //> s5  : String => Any = (c,(List(),(b,3),[empty]),[empty]),(c,[empty]),(a,(b,3
                                                  //| ),[empty]),(a,[empty]),(a,2),[empty]
  s5("c")                                         //> res14: Any = (List(),(b,3),[empty]),[empty]
  find(s5, r1)                                    //> res15: Any = (b,3),[empty]
  find(s5, r3)                                    //> res16: Any = 3
  val r5 = List("c", "b")                         //> r5  : List[String] = List(c, b)
  find(s5, r5)                                    //> res17: Any = 3
  find(s5, List("a", "c"))                        //> res18: Any = [undefined]
  find(s5, List("c", "a"))                        //> res19: Any = [undefined]
  
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