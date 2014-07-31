package org.sfp.lang

class Parameter(val id: String, val t: T) {
  override def toString = id + ":" + t
}