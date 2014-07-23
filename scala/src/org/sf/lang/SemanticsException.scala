package org.sf.lang

/**
 * A class represents an exception on the semantics operations.
 */
class SemanticsException(val msg: String, val code: Integer) extends Exception {
  override def toString = msg
}