package org.sf.lang

/**
 * A class to encapsulate a link reference.
 */
class LinkReference(val ref: Reference, val option: Any = Store.undefined) {
  override def toString = "link[" + ref + "]"
}
