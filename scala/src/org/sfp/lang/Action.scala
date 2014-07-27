package org.sfp.lang

import org.sf.lang.Store

class Action (val parameters: List[Parameter],
              val cost: Integer,
              val precondition: Constraint,
              val effect: Effect) {

  def apply(s: Store) = ???
  
  def simplify(s: Store) = ???
  
  def ground: List[Action] = ???
  
  def toSAS: String = ???
  
  override def toString = {
    val params = parameters.foldRight[String]("(params")(
      (p: Parameter, s: String) => s + " (" + p + ")"
    ) + ")"
    
    "(action " + params + " (cost " + cost + ") (pre " + precondition + ") (eff " + effect + "))"
  }
}