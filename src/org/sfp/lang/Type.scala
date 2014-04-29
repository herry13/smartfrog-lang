package org.sfp.lang

import scala.collection.immutable.HashMap
import org.sf.lang.Reference

class T(val name: String, val isref: Boolean = false, val isvec: Boolean = false) {
  override def equals(t: Any): Boolean = {
    if (!t.isInstanceOf[T]) return false
    val t1 = t.asInstanceOf[T]
    (t1.name.equals(this.name) && t1.isref == this.isref && t1.isvec == this.isvec)
  }
}

object Type {
  //type T = String
  
  // key is the variable's name
  // value is the variable's type
  type Vars = Map[String, T]
  
  // key is the type (int, bool, num, ...)
  // value is the set of super types
  type Types = Map[T, Set[T]]
  
  // a type environment is a pair of sub-typing (Types) and variables' types (Vars)
  type Environment = (Types, Vars)
  
  // In e, set variable r with type t.
  def set(e: Environment, r: Reference, t: T): Environment = {
      val t1 = e._2.get(r.toString)
      if (t1.isEmpty) (e._1, e._2 + (r.toString -> t))
      else throw new TypeError("variable " + r + " is already defined") //(e._1, e._2.updated(r.toString, v.get + t))
    }
  
  // True if r has type t in environment e.
  /*def hastype(e: Environment, r: Reference, t: T): Boolean = {
      val v = e._2.get(r.toString)
      (!v.isEmpty && v.get.contains(t))
    }*/
  
  def typeof(e: Environment, r: Reference): T = {
    val t = e._2.get(r.toString)
    if (t.isEmpty) null
    else t.get
  }

  def hasvar(e: Environment, r: Reference): Boolean = !e._2.get(r.toString).isEmpty
  
  // True if t1 is sub-type of t2 in environment e, otherwise false.
  def subtype(e: Environment, t1: T, t2: T): Boolean = {
      val st1 = e._1.get(t1)
      val st2 = e._1.get(t2)
      if (st1.isEmpty) throw new TypeError("err201: type " + t1 + " is not exist")
      else if (st2.isEmpty) throw new TypeError("err202: type " + t2 + " is not exist")
      else st1.get.contains(t2)
    }
  
  // In environment e, set t1 as the sub-type of t2.
  def setsubtype(e: Environment, t1: T, t2: T): Environment = {
      val st1 = e._1.get(t1)
      val st2 = e._1.get(t2)
      if (st2.isEmpty) throw new TypeError("err204: type " + t2 + " is not exist")
      else if (st1.isEmpty) (e._1 + (t1 -> (st2.get + t1)), e._2)
      else (e._1.updated(t1, st1.get ++ st2.get), e._2)
    }
  
  // In environment e, define type t.
  def define(s: Environment, t: T): Environment = {
      val st = s._1.get(t)
      if (st.isEmpty) (s._1 + (t -> Set(t)), s._2)
      else s
    }
  
  // Create an empty type environment.
  def empty: Environment = (Map(), Map())
  
  // Initialize type environment.
  def init = //: Environment =
    List(_Tbool, _Tnum, _Tstr, _Tobject, _Tschema, _Tconstraint, _Taction).foldRight[Environment](empty)(
      (t: T, e: Environment) => define(e, t)
    )

  // default types
  val _Tbool = new T("bool")
  val _Tnum = new T("num")
  val _Tstr = new T("str")
  val _Tobject = new T("object")
  val _Tschema = new T("schema")
  val _Tconstraint = new T("constraint")
  val _Taction = new T("action")
  val _Tref = new T("*", true)
  val _Tnull = new T("null", true)
  val _Tvec = new T("[]", false, true)
}

class TypeError(val msg: String = "") extends Exception {
  override def toString = msg
}