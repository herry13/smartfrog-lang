package org.sfp.lang

import scala.collection.immutable.HashMap
import org.sf.lang.Reference

object Type {
  // key is the variable's name
  // value is the variable's type
  type Vars = Map[String, Set[String]]
  
  // key is the type (int, bool, num, ...)
  // value is the set of super types
  type Types = Map[String, Set[String]]
  
  // a type environment is a pair of sub-typing (Types) and variables' types (Vars)
  type Environment = (Types, Vars)
  
  // In e, set variable r with type t.
  def set(e: Environment, r: Reference, t: String): Environment = {
      val v = e._2.get(r.toString)
      if (v.isEmpty) (e._1, e._2 + (r.toString -> Set(t)))
      else (e._1, e._2.updated(r.toString, v.get + t))
    }
  
  // True if r has type t in environment e.
  def hastype(e: Environment, r: Reference, t: String): Boolean = {
      val v = e._2.get(r.toString)
      (!v.isEmpty && v.get.contains(t))
    }
  
  // True if t1 is sub-type of t2 in environment e, otherwise false.
  def subtype(e: Environment, t1: String, t2: String): Boolean = {
      val st1 = e._1.get(t1)
      val st2 = e._1.get(t2)
      if (st1.isEmpty) throw new TypeError("err201: type " + t1 + " is not exist")
      else if (st2.isEmpty) throw new TypeError("err202: type " + t2 + " is not exist")
      else st1.get.contains(t2)
    }
  
  // In environment e, set t1 as the sub-type of t2.
  def setsubtype(e: Environment, t1: String, t2: String): Environment = {
      val st1 = e._1.get(t1)
      val st2 = e._1.get(t2)
      if (st2.isEmpty) throw new TypeError("err204: type " + t2 + " is not exist")
      else if (st1.isEmpty) (e._1 + (t1 -> (st2.get + t1)), e._2)
      else (e._1.updated(t1, st1.get ++ st2.get), e._2)
    }
  
  // In environment e, define type t.
  def define(s: Environment, t: String): Environment = {
      val st = s._1.get(t)
      if (st.isEmpty) (s._1 + (t -> Set(t)), s._2)
      else s
    }
  
  // Create an empty type environment.
  def empty: Environment = (Map(), Map())
  
  // Initialize type environment.
  def init = //: Environment =
    List(_bool, _num, _str, _object, _constraint, _action).foldRight[Environment](empty)(
      (t: String, e: Environment) => define(e, t)
    )

  // default types
  val _bool = "bool"
  val _num = "num"
  val _str = "str"
  val _object = "object"
  val _constraint = "constraint"
  val _action = "action"
  val _ref = "ref "
    
  def ref(t: String) = _ref + t
  def isref(t: String) = t.substring(0, 4).equals(_ref)
}

class TypeError(val msg: String = "") extends Exception {
  override def toString = msg
}