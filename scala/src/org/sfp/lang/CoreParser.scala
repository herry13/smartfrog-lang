package org.sfp.lang

import scala.io.Source

import org.sf.lang.Reference
import org.sf.lang.LinkReference
import org.sf.lang.SemanticsException
import org.sf.lang.Store

trait CoreParser extends SfParser {
  protected override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

  def Sfp: Parser[Store] =
    SfpContext ^^ (sc => {
        val r = new Reference("main")
        val s1 = sc(Store.empty)
        val v1 = s1.find(r)
        val s2 = if (v1.isInstanceOf[Store]) s1.accept(r, v1.asInstanceOf[Store], r)
                 else throw new SemanticsException("[err11]", 11)
        val v2 = s2.find(r)
        val rg = new Reference("global")
        val vg = s1.find(rg)
        if (v2.isInstanceOf[Store]) {
          if (vg == Store.undefined) v2.asInstanceOf[Store]
          else if (vg.isInstanceOf[Constraint]) v2.asInstanceOf[Store].bind(rg, vg)
          else throw new SemanticsException("[err202]", 202)
        }
        else throw new SemanticsException("[err11]", 11)
	  }
    )
  
  def SfpContext: Parser[Store => Store] =
    ( "schema" ~> Schema ~ SfpContext ^^ { case schema ~ sc => (s: Store) => sc(schema(s)) }
    | "global" ~> Global ~ SfpContext ^^ { case global ~ sc => (s: Store) => sc(global(s)) }
    | Assignment ~ SfpContext ^^ { case a ~ sc => (s: Store) => sc(a(org.sf.lang.Reference.empty, s)) }
    | epsilon ^^ (x => (s: Store) => s)
    )

  def Block: Parser[(Reference, Store) => Store] =
    ( "include" ~> stringLiteral ~ ";" ~ Block ^^ { case file ~ _ ~ b =>
      	(ns: Reference, s: Store) => {
      	  val filepath = file.substring(1, file.length-1)
      	  if (ns.length == 0) b(ns, parseIncludeFileAtRoot(filepath, s))
      	  else b(ns, parseIncludeFileInBlock(filepath, ns, s))
      	}
      }
    | "global" ~> Global ~ Block ^^ { case g ~ b =>
        (ns: Reference, s: Store) => b(ns, g(s))
      }
    | Assignment ~ Block ^^ { case a ~ b =>
        (ns: Reference, s: Store) => b(ns, a(ns, s))
      }
    | epsilon ^^ { x => (ns: Reference, s: Store) => s }
	)
      
  def Assignment: Parser[(Reference, Store) => Store] =
    ( "def" ~> Reference ~ Action ^^ { case r ~ a =>
      	(ns: Reference, s: Store) => s.bind(ns ++ r, a)
      }
    | Reference ~ TypeValue ~ Value ^^ { case r ~ tv ~ v =>
        (ns: Reference, s: Store) => v(ns, ns ++ r, s)
      }
    )
  
  def Value: Parser[(Reference, Reference, Store) => Store] =
    ( '=' ~> BasicValue <~ eos ^^ (bv =>
        (ns: Reference, r: Reference, s: Store) => s.bind(r, bv)
      )
    | LinkReference <~ eos ^^ (lr =>
        (ns: Reference, r: Reference, s: Store) => s.bind(r, lr(r))
      )
    | SuperSchemaO ~ Prototypes ^^ { case ss ~ ps =>
        (ns: Reference, r: Reference, s: Store) => {
          val sv = ss(r, s.bind(r, Store.empty))
          ps(ns, r, sv)
        }
      }
    )
  
  def SuperSchemaO: Parser[(Reference, Store) => Store] =
    ( "isa" ~> ident ^^ (id => {
      	  val rs = new Reference(id)
      	  (r: Reference, s: Store) => s.inherit(org.sf.lang.Reference.empty, rs, r)
        }
      )
    | epsilon ^^ (x => (r: Reference, s: Store) => s)
    )
  
  def SuperSchemaS: Parser[(Reference, Store) => Store] =
    ( "extends" ~> ident ^^ (id => {
      	  val rs = new Reference(id)
      	  (r: Reference, s: Store) => s.inherit(org.sf.lang.Reference.empty, rs, r)
        }
      )
    | epsilon ^^ (x => (r: Reference, s: Store) => s)
    )

  def Schema: Parser[Store => Store] =
    ident ~ SuperSchemaS ~ "{" ~ Block <~ "}" ^^ { case id ~ ss ~ _ ~ b =>
      (s: Store) => {
      	val r = new Reference(id)
      	val sv = ss(r, s.bind(r, Store.empty))
      	b(r, sv)
      }
    }
  
  //--- type syntax ---//
  // TODO
  def TypeValue: Parser[Any] = ":" ~> Type | epsilon
  
  def Type: Parser[T] =
    ( "[]" ~> tau ^^ (t => new TVec(t))
    | "*" ~> tau ^^ (t => new TRef(t))
    | tau
    )

  def tau: Parser[T] =
    ( "bool"
    | "num"
    | "str"
    | "obj"
    | ident
    ) ^^ (t => new Tau(t))
    
  // same as SF

  def parse(s: String): Store = {
    parseAll(Sfp, s) match {
      case Success(root, _) => root
      case NoSuccess(msg, next) => throw new SemanticsException("[err0] invalid statement at " + next.pos, 0)
    }
  }
  
  def parseIncludeFileAtRoot(filePath: String, s: Store): Store = {
    parseAll(SfpContext, Source.fromFile(filePath).mkString) match {
      case Success(sfp, _) => sfp(s)
      case NoSuccess(msg, next) => throw new SemanticsException("[err0] invalid statement at " + next.pos, 0)
    }
  }
  
  def parseIncludeFileInBlock(filePath: String, ns: Reference, s: Store): Store = {
    parseAll(Block, Source.fromFile(filePath).mkString) match {
      case Success(b, _) => b(ns, s)
      case NoSuccess(msg, next) => throw new SemanticsException("[err0] invalid statement at " + next.pos, 0)
    }
  }
    
  def parseFile(filePath: String): Store = parse(Source.fromFile(filePath).mkString)

  def Action: Parser[Action]
  def Global: Parser[Store => Store]
}
