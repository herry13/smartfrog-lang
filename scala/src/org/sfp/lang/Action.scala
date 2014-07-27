package org.sfp.lang

import org.sf.lang.Store
import org.sfp.lang.Domain._

class Action (val parameters: List[Parameter],
              val cost: Integer,
              val precondition: Constraint,
              val effect: Effect) {

  def apply(s: Store) = ???
  
  def simplify(s: Store) = ???
  
  def ground: List[Action] = ???
  
  def toSAS: String = ???
}

class Parameter(val id: String, val t: T)