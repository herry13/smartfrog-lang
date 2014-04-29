package org.sf.lang

class SemanticsException(val msg: String) extends Exception {
  override def toString = msg
}