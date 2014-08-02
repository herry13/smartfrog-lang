package org.sfp.lang

import org.sf.lang.Reference
import org.sf.lang.LinkReference
import org.sf.lang.SemanticsException
import org.sf.lang.Store

trait ActionParser extends GlobalParser {
  def Action: Parser[org.sfp.lang.Action] =
    "(" ~> Parameters ~ ")" ~ "{" ~ Cost ~ "condition" ~ Condition ~ "effect" ~ Effects <~ "}" ^^ {
      case ps ~ _ ~ _ ~ cost ~ _ ~ cond ~ _ ~ eff =>
        new Action(ps, cost, cond, eff)
    }
  
  def Parameters: Parser[List[Parameter]] =
    ( Parameter ~ ("," ~> Parameter).* ^^ { case p ~ ps => p :: ps }
    | epsilon ^^ (x => List())
    )
    
  def Parameter: Parser[Parameter] =
    ident ~ ":" ~ Type ^^ { case id ~ _ ~ t => new Parameter(id, t) }
    
  def Cost: Parser[Integer] =
    ( "cost" ~> "=" ~> Number <~ eos ^^ (n =>
        if (n.isInstanceOf[Integer]) n.asInstanceOf[Integer]
        else throw new SemanticsException("[err301]", 301)
      )
    | epsilon ^^ (x => 1)
    )
    
  def Condition: Parser[Constraint] =
    ( Conjunction
    | epsilon ^^ (x => True)
    )
    
  def Effects: Parser[Effect] =
    "{" ~> Effect.+ <~ "}" ^^ (effs => {
        val eff1 = effs.head(null)
        effs.tail.foldRight[Effect](eff1)(
          (ef: Effect => Effect, effs: Effect) => ef(effs)
        )
      }
    )
    
  def Effect: Parser[Effect => Effect] =
    Reference ~ "=" ~ BasicValue <~ eos ^^ { case r ~ _ ~ bv =>
      (eff: Effect) => new SimpleEffect(r, bv, eff)
    }
}
