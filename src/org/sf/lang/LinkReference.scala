package org.sf.lang

class LinkReference(val ref: Reference, val option: Any = Store.Undefined) {
  override def toString = "link[" + ref + "]"
}