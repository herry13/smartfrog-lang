package org.sfp.lang

import scala.util.parsing.combinator.JavaTokenParsers
import scala.io.Source

import org.sf.lang.Reference
import org.sf.lang.LinkReference
import org.sf.lang.SemanticsException
import org.sf.lang.Store

trait SfParser extends JavaTokenParsers {
  def Block: Parser[(Reference, Store) => Store]
  
  //==>> same as SF
  def Prototypes: Parser[(Reference, Reference, Store) => Store] =
    ( Prototype ~ Prototypes.? ^^ { case p ~ ps =>
        (ns: Reference, r: Reference, s: Store) =>
          if (ps.isEmpty) p(ns, r, s)
          else ps.get(ns, r, p(ns, r, s))
      }
    | eos ^^ { x => (ns: Reference, r: Reference, s: Store) => s }
    )
  
  //==>> same as SF
  def Prototype: Parser[(Reference, Reference, Store) => Store] =
    ( "extends" ~> Reference ^^ { case r1 =>
        (ns: Reference, r: Reference, s: Store) => s.inherit(ns, r1, r)
      }
    | "{" ~> Block <~ "}" ^^ { case b =>
        (ns: Reference, r: Reference, s: Store) => b(r, s)
      }
    )  

  //==>> same as SF
  def Reference: Parser[Reference] =
    ident ~ ("." ~> ident).* ^^ {
      case id ~ ids => new Reference(id, org.sf.lang.Reference(ids))
    }
	
  //==>> same as SF
  def DataReference: Parser[Reference] =
    Reference

  //==>> same as SF
  def LinkReference: Parser[Reference => LinkReference] =
    Reference ^^ (rp =>
      (r: Reference) =>
        if (rp.subseteqof(r)) throw new SemanticsException("[err4]", 4)
        else new LinkReference(rp)
    )
    
  //==>> same as SF
  def Vector: Parser[List[Any]] =
    "[" ~>
    ( BasicValue ~ ("," ~> BasicValue).* ^^ { case x ~ xs => x :: xs }
    | epsilon ^^ (x => List())
    ) <~ "]"

  //==>> same as SF
  protected val intRegex = """\-?[0-9]+(?!\.)""".r
  
  //==>> same as SF
  def Number: Parser[Any] =
    floatingPointNumber ^^ (n =>
      if (intRegex.findFirstIn(n).get != n) n.toDouble
      else n.toInt
    )

  //==>> same as SF
  def Null: Parser[Any] = "null" ^^ (x => null)
  
  //==>> same as SF
  def Boolean: Parser[Boolean] =
    ( "true" ^^ (x => true)
    | "false" ^^ (x => false)
    )
  
  //==>> same as SF
  def BasicValue: Parser[Any] =
    ( Boolean
    | Number
    | stringLiteral
    | DataReference
    | Vector
    | Null
    )
  
  def Global: Parser[Store => Store]

  val eos: Parser[Any] = ";" | '\n'
  
  val epsilon: Parser[Any] = ""
}
