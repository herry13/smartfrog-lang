package org.sfp.lang

import scala.collection.mutable.MutableList
import org.sf.lang.Reference

class Env(val head: (Reference, T) = (org.sf.lang.Reference.empty, null), val tail: Env = null) {
  def +(r: Reference, t: T): Env = new Env((r, t), this)
  def get(r: Reference): T =
    if (head._1.equals(r)) head._2
    else if (tail != null) tail.get(r)
    else null
    
  override def toString =
    if (tail == null) head.toString
    else head + "," + tail
}

object T {
  val basic = List("bool", "num", "str", "null", "obj", "act", "glob")
  
  val bool = new Tau("bool")  // (Type Bool)
  val num = new Tau("num")    // (Type Num)
  val str = new Tau("str")    // (Type Str)
  val _null = new Tau("null") // (Type Null)
  val obj = new Tau("obj")    // (Type Object)
  val act = new Tau("act")    // (Type Action)
  val glob = new Tau("glob")  // (Type Global)
  
  object Undefined extends T {
    override def toString = "Undefined"
  }
}

trait T {
  val subs = new MutableList[T]()
  def addSubtype(t: T): Unit = (subs += t)
  // this is a sub-type of that
  def <=(that: T): Boolean = (that == this) || (that.subs.exists((t: T) => t.equals(this)))
  def toRef: Reference = throw new TypeError(101, "not a schema")
}

class Tau(val name: String) extends T {
  override def equals(that: Any) = {
    if (that.isInstanceOf[Tau]) that.asInstanceOf[Tau].name.equals(this.name)
    else false
  }
  override def toString = name
  override def toRef =
    if (T.basic.exists((t: String) => t.equals(name))) throw new TypeError(101, "not a schema")
    else new Reference(name)
}

class TList(val t: T) extends T {
  override def equals(that: Any) = {
    if (that.isInstanceOf[TList]) that.asInstanceOf[TList].t.equals(t)
    else false
  }
  override def toString = "[]" + t.toString
}

class TRef(val t: T) extends T {
  override def equals(that: Any) = {
    if (that.isInstanceOf[TRef]) that.asInstanceOf[TRef].t.equals(t)
    else false
  }
  override def toString = "*" + t.toString
}

class TypeError(val code: Integer, val msg: String) extends Exception {
  override def toString = "TypeError[" + code + "]: " + msg
}