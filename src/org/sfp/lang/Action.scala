package org.sfp.lang

import org.sf.lang.Reference

class Action (val params: Map[String, Reference],
			  val cost: Integer,
			  val conditions: Expression,
			  val effects: Expression) {
  
  override def toString =
    "(action (" + params + ") " + cost + " (cond " + conditions + ") (eff " + effects + "))"
}