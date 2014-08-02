package org.sfp.lang

import org.sf.lang.Reference
import org.sf.lang.LinkReference
import org.sf.lang.SemanticsException
import org.sf.lang.Store

trait GlobalParser extends CoreParser {
  //--- global ---//
  def Global: Parser[Store => Store] =
    Conjunction ^^ (gc =>
      (s: Store) => {
        val r = new Reference("global")
        val gs = s.find(r)
        if (gs == Store.undefined) s.bind(r, gc)
        else if (gs.isInstanceOf[Constraint]) {
          val f = new Conjunction(List(gs.asInstanceOf[Constraint], gc))
          s.bind(r, f)
        }
        else throw new SemanticsException("[err201] global is not a constraint", 201)
      }
    )
  
  def Conjunction: Parser[Constraint] =
    "{" ~>
    ( ConstraintStatement.+ ^^ (cs => new Conjunction(cs))
    | epsilon ^^ (cs => new Conjunction(List()))
    ) <~ "}"
    
  def Disjunction: Parser[Constraint] =
    "(" ~>
    ( ConstraintStatement.+ ^^ (cs => new Disjunction(cs))
    | epsilon ^^ (cs => new Disjunction(List()))
    ) <~ ")"
    
  def ConstraintStatement: Parser[Constraint] =
    ( Equal
    | NotEqual
    | Negation
    | Implication
    | Conjunction
    | Disjunction
    | MemberOfList
    )
   
  def Equal: Parser[Constraint] =
    Reference ~ "=" ~ BasicValue <~ eos ^^ { case r ~ _ ~ bv =>
      new Equal(r, bv)
    }
  
  def NotEqual: Parser[Constraint] =
    Reference ~ "!=" ~ BasicValue <~ eos ^^ { case r ~ _ ~ bv =>
      new NotEqual(r, bv)
    }
  
  def Implication: Parser[Constraint] = 
    "if" ~> Conjunction ~ "then" ~ Conjunction ^^ { case premise ~ _ ~ conclusion =>
      new Implication(premise, conclusion)
    }
  
  def Negation: Parser[Constraint] =
    "not" ~> ConstraintStatement ^^ (cs =>
      new Negation(cs)
    )
    
  def MemberOfList: Parser[Constraint] =
    Reference ~ "in" ~ Vector <~ eos ^^ { case r ~ _ ~ vec =>
      new MemberOfList(r, vec)
    }
}
