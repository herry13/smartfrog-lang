package org.sfp.lang

import scala.util.parsing.combinator.JavaTokenParsers
import scala.io.Source

import org.sf.lang.Reference
import org.sf.lang.LinkReference
import org.sf.lang.SemanticsException
import org.sf.lang.Store

/**
 * The main object of sfParser application.
 */
object Parser extends Parser with App {
  val help = """Usage: scala org.sfp.lang.Parser [option] <sfp-file>

where [option] is:
  -json     print output in JSON
  -yaml     print output in YAML
"""
    
  if (args.length <= 0) Console.println(help)
  else {
    try {
      if (isInclude(args)) println(parseIncludeFile(args.tail.head, org.sf.lang.Reference.empty, Store.empty))
      else if (inJson(args)) println(parseFile(args.tail.head).toJson)
      else if (inYaml(args)) println(parseFile(args.tail.head).toYaml)
      else println(parseFile(args.head))
    } catch {
      case se: SemanticsException => System.err.println(se.msg)
      case e: Exception => e.printStackTrace()
    }
  }
    
  def inJson(args: Array[String]): Boolean =
    (args.length >= 2 && args.head.equals("-json"))
    
  def inYaml(args: Array[String]): Boolean =
    (args.length >= 2 && args.head.equals("-yaml"))
    
  def isInclude(args: Array[String]): Boolean =
    (args.length >= 2 && args.head.equals("-i"))
}

class Parser extends JavaTokenParsers {
  import Domain._
  
  protected override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

  def Sfp: Parser[Store] =
    SfpContext ^^ (sc => {
        val r = new Reference("main")
        val s1 = sc(org.sf.lang.Reference.empty, Store.empty)
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
  
  def SfpContext: Parser[(Reference, Store) => Store] =
    ( "schema" ~> Schema ~ SfpContext ^^ { case schema ~ sc => (ns: Reference, s: Store) => sc(ns, schema(s)) }
    | "global" ~> Global ~ SfpContext ^^ { case global ~ sc => (ns: Reference, s: Store) => sc(ns, global(s)) }
    | Assignment ~ SfpContext ^^ { case a ~ sc => (ns: Reference, s: Store) => sc(ns, a(ns, s)) }
    | epsilon ^^ (x => (ns: Reference, s: Store) => s)
    )
  
  //==>> same as SF
  def Body: Parser[(Reference, Store) => Store] =
    ( "include" ~> stringLiteral ~ ";" ~ Body ^^ { case file ~ _ ~ b =>
      	(ns: Reference, s: Store) => b(ns, parseIncludeFile(file.substring(1, file.length-1), ns, s))
      }
    | "global" ~> Global ~ Body ^^ { case g ~ b =>
        (ns: Reference, s: Store) => b(ns, g(s))
      }
	| Assignment ~ Body ^^ { case a ~ b =>
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
  
  //==>> same as SF
  def Prototypes: Parser[(Reference, Reference, Store) => Store] =
    ( Prototype ~ ("," ~> Prototypes).? ^^ { case p ~ ps =>
        (ns: Reference, r: Reference, s: Store) =>
          if (ps.isEmpty) p(ns, r, s)
          else ps.get(ns, r, p(ns, r, s))
      }
    | epsilon ^^ { x => (ns: Reference, r: Reference, s: Store) => s }
    )
  
  //==>> same as SF
  def Prototype: Parser[(Reference, Reference, Store) => Store] =
    ( "extends".? ~> Reference ^^ { case r1 =>
        (ns: Reference, r: Reference, s: Store) => s.inherit(ns, r1, r)
      }
    | "{" ~> Body <~ "}" ^^ { case b =>
        (ns: Reference, r: Reference, s: Store) => b(r, s)
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
    ident ~ SuperSchemaS ~ "{" ~ Body <~ "}" ^^ { case id ~ ss ~ _ ~ b =>
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
    tau ~ tauSuffix ^^ { case t ~ ts => t.toString + ts }
  
  def tauSuffix: Parser[String] =
    ( "[]" ^^ (x => "[]")
    | "*" ^^ (x => "*")
    | epsilon ^^ (x => "")
    )

  def tau: Parser[T] =
    ("bool" | "num" | "str" | "obj" | ident) ^^ (x => x)
    
  val eos: Parser[Any] = ";" | '\n'
  
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
    
  def Action: Parser[Action] =
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
    
  // same as SF
  val epsilon: Parser[Any] = ""

  def parse(s: String): Store = {
    parseAll(Sfp, s) match {
      case Success(root, _) => root
      case NoSuccess(msg, next) => throw new SemanticsException("[err0] invalid statement at " + next.pos, 0)
    }
  }
    
  def parseIncludeFile(filePath: String, ns: Reference, s: Store): Store = {
    def helper(element: Parser[(Reference, Store) => Store]): Store = {
      parseAll(element, Source.fromFile(filePath).mkString) match {
    	case Success(el, _) => el(ns, s)
    	case NoSuccess(msg, next) => throw new SemanticsException("[err0] invalid statement at " + next.pos, 0)
      }
    }
    
    if (ns.length > 0) helper(Body)
    else helper(SfpContext)
  }
    
  def parseFile(filePath: String): Store = parse(Source.fromFile(filePath).mkString)
}