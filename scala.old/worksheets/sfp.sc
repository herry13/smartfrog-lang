import org.sfp.lang._
import org.sf.lang.Reference

object sfp {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val t1 = Type.init                              //> t1  : (org.sfp.lang.Type.Types, org.sfp.lang.Type.Vars) = (Map(object -> Set
                                                  //| (object), num -> Set(num), str -> Set(str), bool -> Set(bool), constraint ->
                                                  //|  Set(constraint), action -> Set(action)),Map())
  val t2 = Type.define(t1, "bool")                //> t2  : (org.sfp.lang.Type.Types, org.sfp.lang.Type.Vars) = (Map(object -> Set
                                                  //| (object), num -> Set(num), str -> Set(str), bool -> Set(bool), constraint ->
                                                  //|  Set(constraint), action -> Set(action)),Map())
  val r1 = new Reference("a")                     //> r1  : org.sf.lang.Reference = a
  val t3 = Type.set(t2, r1, "bool")               //> t3  : (org.sfp.lang.Type.Types, org.sfp.lang.Type.Vars) = (Map(object -> Set
                                                  //| (object), num -> Set(num), str -> Set(str), bool -> Set(bool), constraint ->
                                                  //|  Set(constraint), action -> Set(action)),Map(a -> Set(bool)))
  Type.subtype(t3, "bool", "bool")                //> res0: Boolean = true
  Type.subtype(t3, "bool", "num")                 //> res1: Boolean = false
}