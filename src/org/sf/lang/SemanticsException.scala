package org.sf.lang

class SemanticsException(val msg: String = "[todo]") extends Exception {
  override def toString = msg
}