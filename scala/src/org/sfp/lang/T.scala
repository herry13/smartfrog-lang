package org.sfp.lang

import org.sf.lang.Reference

class Env(val head: (Reference, T) = (org.sf.lang.Reference.empty, null), val tail: Env = null) {
  def +(r: Reference, t: T): Env = new Env((r, t), this)  // (Env Var)
  
  def get(r: Reference): T =  // (Res)
    if (isEmpty) null
    else if (head._1.equals(r)) head._2
    else if (tail != null) tail.get(r)
    else null
    
  def exists(t: T): Boolean =
    if (t.isInstanceOf[Tau]) (head._2 == t) || (tail.exists(t))
    else if (t.isInstanceOf[TList]) exists(t.asInstanceOf[TList].tau)  // (Type Vec)
    else if (t.isInstanceOf[TRef]) exists(t.asInstanceOf[TRef].tau)    // (Type Ref)
    else ???
    
  def replacePrefix(prefix: Reference): Env =
    if (isEmpty) null
    else if (tail == null)
      if (prefix < head._1) this else null
    else
      if (prefix < head._1) new Env((head._1 -- prefix, head._2), tail.replacePrefix(prefix))
      else tail.replacePrefix(prefix)
      
  def inherit(src: Reference, dest: Reference): Env = {
    def copy(attrs: Env, e: Env): Env = {
      if (attrs == null || attrs.isEmpty) e
      else {
        val r = dest ++ attrs.head._1
        val tx = e.get(r)
        if (tx == null) copy(attrs.tail, e + (r, attrs.head._2))
        else if (attrs.head._2 <= tx) copy(attrs.tail, e)  // (Assign1)
        else throw new TypeError(201, "inherit: " + attrs.head._2 + "<:" + tx + "=" + (attrs.head._2 <= tx))
      }
    }
    copy(replacePrefix(src), this)
  }
  
  def isEmpty: Boolean = (head._2 == null)
  
  override def toString =
    if (isEmpty) "(empty)"
    else if (tail == null) head.toString
    else head + "," + tail
}

object T {
  val basic = List("bool", "num", "str", "null", "obj", "act", "glob")
  
  val bool = new Tau("bool")  // (Type Bool)
  val num = new Tau("num")    // (Type Num)
  val str = new Tau("str")    // (Type Str)
  val obj = new Tau("obj")    // (Type Object)
  val act = new Tau("act")    // (Type Action)
  val glob = new Tau("glob")  // (Type Global)

  val _null = new Tau("null") {  // (Type Null)
    override def <=(that: T) = {
      (this == that) ||          // (Reflex)
      (that.isInstanceOf[TRef])  // (Ref Null)
    }
  }
  
  object Undefined extends T {
    override def <=(that: T) = (that == this)
    override def toString = "Undefined"
  }
}

trait T {
  def <=(that: T): Boolean
  def toRef: Reference = throw new TypeError(101, "not a schema")
}

class Tau(val name: String, val parent: T = null) extends T {
  override def equals(that: Any) = {
    if (that.isInstanceOf[Tau]) that.asInstanceOf[Tau].name.equals(this.name)
    else false
  }
  override def <=(that: T) = {
    (that == this) || // (Reflex)
    (parent != null && (parent == that || parent <= that) ) // (Trans)
  }
  override def toRef = {
    if (T.basic.exists((t: String) => t.equals(name))) throw new TypeError(101, "not a schema")
    else new Reference(name)
  }
  override def toString = if (parent == null) name else name + "<:" + parent
}

class TList(val tau: T) extends T {
  override def equals(that: Any) = {
    if (that.isInstanceOf[TList]) that.asInstanceOf[TList].tau.equals(tau)
    else false
  }
  override def <=(that: T) = {
    (that == this) || // (Reflex)
    (that.isInstanceOf[TList] && tau <= that.asInstanceOf[TList].tau) // (Vec Subtype)
  }
  override def toString = "[]" + tau.toString
}

class TRef(val tau: T) extends T {
  override def equals(that: Any) = {
    if (that.isInstanceOf[TRef]) that.asInstanceOf[TRef].tau.equals(tau)
    else false
  }
  override def <=(that: T) = {
    (that == this) || // (Reflex)
    (that.isInstanceOf[TRef] && tau <= that.asInstanceOf[TRef].tau) // (Ref Subtype)
  }
  override def toString = "*" + tau.toString
}

class TSchema(val tau: Tau) extends T {
  override def <=(that: T) = (that == this)
  override def toString = "[" + tau + "]"
}

class TypeError(val code: Integer, val msg: String) extends Exception {
  override def toString = "TypeError[" + code + "]: " + msg
}