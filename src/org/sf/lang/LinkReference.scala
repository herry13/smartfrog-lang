package org.sf.lang

class LinkReference(val ref: Reference, val option: Any = Store.undefined) {
  override def toString = "link[" + ref + "]"
}