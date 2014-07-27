package org.sfp.lang

import org.sf.lang.Store
import org.sf.lang.Reference

trait Effect {
  def apply(s: Store): Store
}

class SimpleEffect(val r: Reference, val v: Any, val rest: Effect = null) extends Effect {
  def apply(s: Store): Store = {
    val s1 = s.bind(r, v)
    if (rest == null) s1 else rest(s1)
  }
}

class ConditionalEffect(val r: Reference, val v: Any, val c: Constraint, val rest: Effect = null) extends Effect {
  def apply(s: Store): Store = {
    val s1 = if (c(s)) s.bind(r, v) else s
    if (rest == null) s1 else rest(s1)
  }
}