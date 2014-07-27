package org.sfp.lang

import org.sf.lang.Store
import org.sf.lang.Reference

trait Constraint {
  /**
   * Evaluate the constraint.
   * @param s a store to be evaluated
   * @return true, if a given store satisfies the constraint, otherwise false
   */
  def apply(s: Store): Boolean
  
  // convert the constraint into DNF formula
  def toDNF: Constraint
  
  // convert the constraint into 3-CNF formula
  //def to3CNF: Constraint
  
  // remove nested reference from the constraint
  def normalise(s: Store): Constraint
  
  def toSAS(s: Store): String
}

object True extends Constraint {
  def apply(s: Store) = true
  override def toString = "(false)"

  override def toDNF: Constraint = this
  override def normalise(s: Store): Constraint = this  
  override def toSAS(s: Store): String = ???
}

object False extends Constraint {
  def apply(s: Store) = false
  override def toString = "(true)"

  override def toDNF: Constraint = this
  override def normalise(s: Store): Constraint = this 
  override def toSAS(s: Store): String = ???
}

class Equal(val r: Reference, val v: Any) extends Constraint {
  def apply(s: Store) = s.find(r).equals(v)
  override def toString = "(= " + r + " " + v + ")"

  override def toDNF: Constraint = this
  override def normalise(s: Store): Constraint = ???  
  override def toSAS(s: Store): String = ???
}

class NotEqual(val r: Reference, val v: Any) extends Constraint {
  def apply(s: Store) = !s.find(r).equals(v)
  override def toString = "(not (= " + r + " " + v + "))"

  override def toDNF: Constraint = ???
  override def normalise(s: Store): Constraint = ???  
  override def toSAS(s: Store): String = ???
}

class Implication(val premise: Constraint, val conclusion: Constraint) extends Constraint {
  def apply(s: Store) = (!premise(s) || conclusion(s))
  override def toString = "(imply " + premise + " " + conclusion + ")"

  override def toDNF: Constraint = {
    new Disjunction(List(new Negation(premise), conclusion))
  }
  override def normalise(s: Store): Constraint = ???  
  override def toSAS(s: Store): String = ???
}

class Negation(val c: Constraint) extends Constraint {
  def apply(s: Store) = !c(s)
  override def toString = "(not " + c + ")"

  override def toDNF: Constraint = ???
  override def normalise(s: Store): Constraint = ???  
  override def toSAS(s: Store): String = ???
}

class MemberOfList(val r: Reference, val vec: List[Any]) extends Constraint {
  def apply(s: Store) = {
    val vr = s.find(r)
    vec.exists((v: Any) => v.equals(vr))
  }
  override def toString = "(in " + r + " " + vec + ")"

  override def toDNF: Constraint = {
    val cs = vec.foldRight[List[Constraint]](List())(
      (v: Any, cs: List[Constraint]) => new Equal(r, v) :: cs
    )
    return new Disjunction(cs)
  }
  override def normalise(s: Store): Constraint = ???  
  override def toSAS(s: Store): String = ???
}

class Conjunction(val cs: List[Constraint]) extends Constraint {
  def apply(s: Store) = {
    if (cs.length == 0) true
    else cs.forall((c: Constraint) => c(s))
  }
  override def toString = {
    cs.foldRight[StringBuffer](new StringBuffer("(and"))(
      (c: Constraint, sb: StringBuffer) => sb.append(" ").append(c.toString)
    ).append(")").toString
  }

  override def toDNF: Constraint = ???
  override def normalise(s: Store): Constraint = ???  
  override def toSAS(s: Store): String = ???
}

class Disjunction(val cs: List[Constraint]) extends Constraint {
  def apply(s: Store) = {
    if (cs.length == 0) true
    else cs.exists((c: Constraint) => c(s))
  }
  override def toString = {
    cs.foldRight[StringBuffer](new StringBuffer("(or"))(
      (c: Constraint, sb: StringBuffer) => sb.append(" ").append(c.toString)
    ).append(")").toString
  }

  override def toDNF: Constraint = ???
  override def normalise(s: Store): Constraint = ???  
  override def toSAS(s: Store): String = ???
}