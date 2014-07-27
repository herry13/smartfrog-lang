package org.sfp.lang

import org.sf.lang.Reference

object Type {
  type Environment = Reference => org.sfp.lang.Type
  
  def define(r: Reference, t: Type): Environment =
    (r1: Reference) => if (r.equals(r1)) t else null
    
  def define(r: Reference, t: Type, e: Environment): Environment =
    (r1: Reference) => if (r.equals(r1)) t else e(r1)
}

trait Type {
}

class BasicType(val name: String) extends Type {
  override def equals(t: Any) = {
    if (t.isInstanceOf[BasicType]) t.asInstanceOf[BasicType].name.equals(this.name)
    else false
  }
  override def toString = name
}

class ListType(val t: Type) extends Type {
  override def equals(t: Any) = {
    if (t.isInstanceOf[ListType]) t.asInstanceOf[ListType].t.equals(t)
    else false
  }
  override def toString = "[]" + t.toString
}

class RefType(val t: Type) extends Type {
  override def equals(t: Any) = {
    if (t.isInstanceOf[RefType]) t.asInstanceOf[RefType].t.equals(t)
    else false
  }
  override def toString = "*" + t.toString
}