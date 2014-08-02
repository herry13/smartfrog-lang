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
    else if (t.isInstanceOf[TVec]) exists(t.asInstanceOf[TVec].tau)  // (Type Vec)
    else if (t.isInstanceOf[TRef]) exists(t.asInstanceOf[TRef].tau)    // (Type Ref)
    else ???
    
  def exists(r: Reference) = get(r) != null
    
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

  def eval(e: Env): Env = {
    val e1 =
      if (head._2.isInstanceOf[TForward]) {
        val t = head._2.asInstanceOf[TForward].eval(e)
        if (t == null) throw new TypeError(501, "type of " + head._1 + " cannot be determined")
        else e + (head._1, t)
      }
      else e
    if (tail == null) e1 else tail.eval(e1)
  }
  
  override def toString =
    if (isEmpty) "(empty)"
    else if (tail == null) head.toString
    else head + "\n" + tail
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
  
  val x = List(y)
  val y = 1
}

trait T {
  def <=(that: T): Boolean
  def toRef: Reference = throw new TypeError(101, "not a schema")
}

trait TForward extends T {
  var tlist: List[Any] = List()
  override def <=(that: T) = false
  def plausible(that: T) = true
  def add(that: T): Unit = tlist ++= List(that)
  def add(that: Reference): Unit = tlist ++= List(that)
  def eval(e: Env): T = {
    val t1 = getType(tlist.head, e)
    if (tlist.forall((x: Any) => getType(x, e) <= t1)) t1
    else throw new TypeError(502, "vector elements have incompatible types")
  }
  def getType(v: Any, e: Env): T
}

class TDataRef extends TForward {
  override def plausible(that: T) = (that.isInstanceOf[TRef] || that.isInstanceOf[TDataRef])
  override def toString = "*?" + tlist
  override def getType(item: Any, e: Env): T = {
    if (item.isInstanceOf[T]) item.asInstanceOf[T]
    else if (item.isInstanceOf[Reference]) {
      val t_item = e.get(item.asInstanceOf[Reference])
      if (t_item.isInstanceOf[TForward]) new TRef(t_item.asInstanceOf[TForward].eval(e))
      else new TRef(t_item)
    }
    else ???
  }
}

class TLinkRef extends TForward {
  override def toString = "!" + tlist
  override def getType(item: Any, e: Env): T = {
    if (item.isInstanceOf[T]) item.asInstanceOf[T]
    else if (item.isInstanceOf[Reference]) {
      val t_item = e.get(item.asInstanceOf[Reference])
      if (t_item.isInstanceOf[TForward]) t_item.asInstanceOf[TForward].eval(e)
      else t_item
    }
    else ???
  }
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

class TVec(val tau: T) extends T {
  override def equals(that: Any) = {
    if (that.isInstanceOf[TVec]) that.asInstanceOf[TVec].tau.equals(tau)
    else false
  }
  override def <=(that: T) = {
    (that == TVec.this) ||                                          // (Reflex)
    (that.isInstanceOf[TVec] && tau <= that.asInstanceOf[TVec].tau) // (Vec Subtype)
  }
  override def toString = "[]" + tau.toString
}

class TRef(val tau: T) extends T {
  assert(!tau.isInstanceOf[TForward], "a reference of a forward reference is not allowed")
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
  override def toString = "$" + tau
}

class TypeError(val code: Integer, val msg: String) extends Exception {
  override def toString = "TypeError[" + code + "]: " + msg
}