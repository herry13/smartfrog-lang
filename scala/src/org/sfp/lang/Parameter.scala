package org.sfp.lang

class Parameter(val id: String, val t: Type) {
  override def toString = id + ":" + t
}