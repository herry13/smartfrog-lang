object testsfp1 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  /*type X = Map[String, Integer]
  
  val a : X = Map()
  a + (("x", 1))
   
  val t1 = new org.sfp.lang.Tau("int")
  val t2 = new org.sfp.lang.Tau("binary")
  t1.addSubtype(t2)
  t1.subs
  t1.subs.exists((t: org.sfp.lang.T) => t.equals(t2))
                                                  
  org.sfp.lang.T.Undefined == org.sfp.lang.T.Undefined*/
  
  val e = new org.sfp.lang.Env()                  //> e  : org.sfp.lang.Env = (empty)
  val r = new org.sf.lang.Reference("a")          //> r  : org.sf.lang.Reference = a
  val e1 = e + (r, org.sfp.lang.T.num)            //> e1  : org.sfp.lang.Env = (a,num)
                                                  //| (empty)
  e1.get(r)                                       //> res0: org.sfp.lang.T = num
  val r1 = new org.sf.lang.Reference("a")         //> r1  : org.sf.lang.Reference = a
  e1.get(r1)                                      //> res1: org.sfp.lang.T = num

  r.equals(r1)                                    //> res2: Boolean = true
  
}