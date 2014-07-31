package org.sfp.lang

import scala.util.parsing.combinator.JavaTokenParsers
import scala.io.Source

import org.sf.lang.Reference
import org.sf.lang.LinkReference
import org.sf.lang.SemanticsException
import org.sf.lang.SyntaxError

object TParser extends TParser with App {
  val help = """Usage: scala org.sfp.lang.TParser [option] <sfp-file>
"""
    
  if (args.length <= 0) Console.println(help)
  else {
    try {
      println(this.parseFile(args.head))
    } catch {
      case te: TypeError => System.err.println(te)
      case e: Exception => e.printStackTrace()
    }
  }
}

class TParser extends JavaTokenParsers {
  import T._
  
  protected override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r
  val ref_global = new Reference("global")
  
  def Sfp: Parser[Env] =
    SfpContext ^^ (sc => {
        /*val r = new Reference("main")
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
        else throw new SemanticsException("[err11]", 11)*/
        
        sc(new Env())
	  }
    )
  
  def SfpContext: Parser[Env => Env] =
    ( "schema" ~> Schema ~ SfpContext ^^ { case s ~ sc =>
        (e: Env) => sc(s(e))
      }
    | "global" ~> Global ~ SfpContext ^^ { case g ~ sc =>
        (e: Env) => {
          val tg = glob  // (Glob)
          sc(g(e + (ref_global, tg)))
        }
      }
    | Assignment ~ SfpContext ^^ { case a ~ sc =>
        (e: Env) => sc(a(org.sf.lang.Reference.empty, e))
      }
    | epsilon ^^ (x => (e: Env) => e)
    )
  
  def Block: Parser[(Reference, Env) => Env] =
    ( "include" ~> stringLiteral ~ eos ~ Block ^^ { case file ~ _ ~ b =>
        (ns: Reference, e: Env) => {
          val filepath = file.substring(1, file.length-1)
          if (ns.length == 0) b(ns, parseIncludeFileAtRoot(filepath, e))
          else b(ns, parseIncludeFileInBlock(filepath, ns, e))
        }
      }
    | "global" ~> Global ~ Block ^^ { case g ~ b =>
        (ns: Reference, e: Env) => {
          val tg = glob  // (Glob)
          b(ns, g(e + (ref_global, tg)))
        }
      }
	| Assignment ~ Block ^^ { case a ~ b =>
        (ns: Reference, e: Env) => b(ns, a(ns, e))
      }
	| epsilon ^^ { x => (ns: Reference, e: Env) => e }
	)
   
  /**
   * Error codes: 1x
   */
  def Assignment: Parser[(Reference, Env) => Env] =
    ( "def" ~> Reference ~ Action ^^ { case rvar ~ a =>
        (ns: Reference, e: Env) => {
          val r = ns ++ rvar
          val tr = e.get(r)
          val ta = act // (Act)
          val er =
            if (tr == null) e + (r, act)  // (Assign1)
            else if (tr <= ta) e      // (Assign2)
            else throw new TypeError(11, tr + " <- " + act)
          a(er)
        }
      }
    | Reference ~ TypeValue ~ Value ^^ { case rvar ~ tt ~ v =>
        (ns: Reference, e: Env) => {
          val r = ns ++ rvar
          if (tt == null) v(r, e)  // (Assign1) & (Assign2)
          else {
            val tr = e.get(r)
            if (tr == null) v(r, e + (r, tt))  // (Assign3)
            else if (tt <= tr) v(r, e)         // (Assign4)
            else throw new TypeError(12, r + ": " + tt + "<:" + tr + "=" + (tt <= tr))
          }
        }
      }
    )
  
  def Value: Parser[(Reference, Env) => Env] =
    ( "=" ~> BasicValue <~ eos ^^ (bv => // TODO -- forward data reference
        (r: Reference, e: Env) => {
          val tr = e.get(r)
          if (tr == null) e + (r, bv)  // (Assign1,2)
          else if (bv <= tr) e         // (Assign3,4)
          else throw new TypeError(13, r + ": " + bv + "<:" + tr + "=" + (bv <= tr))
        }
      )
    | LinkReference <~ eos ^^ (l => // TODO -- forward link reference
        (r: Reference, e: Env) => {
          val tr = e.get(r)
          val (rl, tl) = l(r, e)
          if (tr == null) (e + (r, tl)).inherit(rl, r)  // (Assign1,2)
          else if (tl <= tr) e                          // (Assign3,4)
          else throw new TypeError(14, r + ": " + tl + "<:" + tr + "=" + (tl <= tr))
        }
      )
    | SuperSchemaO ~ Prototypes ^^ { case ss ~ ps => // TODO
        (r: Reference, e: Env) => ??? //ps(r, ss, e)
      }
    )

  /*
  // TODO 
  def Prototypes: Parser[Env => Env] =
    ( Prototype ~ ("," ~> Prototypes).? ^^ { case p ~ ps =>
        ??? /*(ns: Reference, r: Reference, s: Store) =>
          if (ps.isEmpty) p(ns, r, s)
          else ps.get(ns, r, p(ns, r, s))*/
      }
    | epsilon ^^ { x => (e: Env) => ??? }
    )
  
  // TODO 
  def Prototype: Parser[(Reference, Env) => Env] =
    ( "extends".? ~> Reference ^^ { case r1 =>
        ??? //(ns: Reference, r: Reference, s: Store) => s.inherit(ns, r1, r)
      }
    | "{" ~> Block <~ "}" ^^ { case b =>
        ??? //(ns: Reference, r: Reference, s: Store) => b(r, s)
      }
    )
  */
  def Prototypes: Parser[(Reference, String, Env) => Env] =
    ( "{" ~> Block ~ "}" ~ Prototypes ^^ { case b ~ _ ~ ps => ??? }
    | "extends" ~> Reference ~ Prototypes ^^ { case r ~ ps => ??? }
    | eos ^^ (x => ???)
    )
    
  // TODO 
  def SuperSchemaO: Parser[String] =
    ( "isa" ~> ident
    | epsilon ^^ (x => null)
    )
      
  def Reference: Parser[Reference] =
    ident ~ ("." ~> ident).* ^^ {
      case id ~ ids => new Reference(id, org.sf.lang.Reference(ids))
    }
	
  def DataReference: Parser[T] =
    Reference ^^ (r => ???) // TODO

  def LinkReference: Parser[(Reference, Env) => (Reference, T)] =
    Reference ^^ (rp =>
      (r: Reference, e: Env) =>
        if (rp.subseteqof(r)) throw new SemanticsException("[err4]", 4)
        else {
          val tr = e.get(r)
          if (tr != null) (r, tr)
          else ??? // TODO
        }
    )
    
  /**
   * Error codes: 2x
   */
  def Vector: Parser[T] =
    "[" ~>
    ( BasicValue ~ ("," ~> BasicValue).* ^^ { case x ~ xs => {
          // (Vec)
          val t = x
          if (xs.length > 0) xs.forall(
            (tx: T) =>
              if (tx.equals(t)) true
              else throw new TypeError(21, "vec - " + t + "!=" + tx)
          )
          new TList(t)
        }
      }
    | epsilon ^^ (x => Undefined)
    ) <~ "]"

  def Number: Parser[T] =
    floatingPointNumber ^^ (n => num) // (Num)

  def Null: Parser[T] = "null" ^^ (x => _null) // (Null)
  
  def Boolean: Parser[T] =
    ( "true" ^^ (x => bool)  // (Bool)
    | "false" ^^ (x => bool) // (Bool)
    )
  
  def BasicValue: Parser[T] =
    ( Boolean
    | Number
    | stringLiteral ^^ (t => str) // (Str)
    | Null
    | Vector
    | DataReference
    )
  
  /**
   * Error codes: 3x
   */
  def SuperSchemaS: Parser[(String, Env) => Env] =
    ( "extends" ~> ident ^^ (ids =>
        (id: String, e: Env) => {
          val rid = new Reference(id)
          val tid = e.get(rid)
          if (tid != null) {
            if (tid.isInstanceOf[TSchema]) throw new TypeError(31, "schema " + id + " is already exist")
            else throw new TypeError(32, id + " is already exist and not a schema")
          }
          // (Type Schema)
          val rids = new Reference(ids)
          val tids = e.get(rids)
          if (!tids.isInstanceOf[TSchema]) throw new TypeError(33, "schema " + ids + " is not exist")
          // (Schema Subtype)
          val e1 = e + (rid, new TSchema(new Tau(id, tids)))
          // inherit attributes from super schema
          e1.inherit(rids, rid)
        }
      )
    | epsilon ^^ (x =>
        (id: String, e: Env) => {
          val rid = new Reference(id)
          val tid = e.get(rid)
          if (tid != null) {
            if (tid.isInstanceOf[TSchema]) throw new TypeError(31, "schema " + id + " is already exist")
            else throw new TypeError(32, id + " is already exist and not a schema")
          }
          // (Type Schema) & (Object Subtype)
          e + (rid, new TSchema(new Tau(id, obj)))
        }
      )
    )

  def Schema: Parser[Env => Env] =
    ident ~ SuperSchemaS ~ "{" ~ Block <~ "}" ^^ { case id ~ ss ~ _ ~ b =>
      (e: Env) => b(new Reference(id), ss(id, e))
    }
  
  //--- type syntax ---//
  def TypeValue: Parser[T] =
    ( ":" ~> Type
    | epsilon ^^ (t => null)
    )
  
  def Type: Parser[T] =
    ( "[]" ~> tau ^^ (t => new TList(t))
    | "*" ~> tau ^^ (t => new TRef(t))
    | tau
    )

  def tau: Parser[T] =
    ( "bool" ^^ (t => bool)
    | "num" ^^ (t => num)
    | "str" ^^ (t => str)
    | "obj" ^^ (t => obj)
    | ident ^^ (id => new Tau(id))
    )
    
  //--- global ---//
  // TODO 
  def Global: Parser[Env => Env] =
    Conjunction ^^ (gc =>
      /*(s: Store) => {
        val r = new Reference("global")
        val gs = s.find(r)
        if (gs == Store.undefined) s.bind(r, gc)
        else if (gs.isInstanceOf[Constraint]) {
          val f = new Conjunction(List(gs.asInstanceOf[Constraint], gc))
          s.bind(r, f)
        }
        else throw new SemanticsException("[err201] global is not a constraint", 201)
      }*/
      ???
    )
  
  // TODO 
  def Conjunction: Parser[Env => Env] =
    "{" ~>
    ( ConstraintStatement.+ ^^ (cs => ???) //new Conjunction(cs))
    | epsilon ^^ (cs => ???) //new Conjunction(List()))
    ) <~ "}"
    
  // TODO 
  def Disjunction: Parser[Env => Env] =
    "(" ~>
    ( ConstraintStatement.+ ^^ (cs => ???) //new Disjunction(cs))
    | epsilon ^^ (cs => ???) //new Disjunction(List()))
    ) <~ ")"
    
  // TODO 
  def ConstraintStatement: Parser[Env => Env] =
    ( Equal
    | NotEqual
    | Negation
    | Implication
    | Conjunction
    | Disjunction
    | MemberOfList
    )
   
  // TODO 
  def Equal: Parser[Env => Env] =
    Reference ~ "=" ~ BasicValue <~ eos ^^ { case r ~ _ ~ bv =>
      ??? //new Equal(r, bv)
    }
  
  // TODO 
  def NotEqual: Parser[Env => Env] =
    Reference ~ "!=" ~ BasicValue <~ eos ^^ { case r ~ _ ~ bv =>
      ??? //new NotEqual(r, bv)
    }
  
  // TODO 
  def Implication: Parser[Env => Env] = 
    "if" ~> Conjunction ~ "then" ~ Conjunction ^^ { case premise ~ _ ~ conclusion =>
      ??? //new Implication(premise, conclusion)
    }
  
  // TODO 
  def Negation: Parser[Env => Env] =
    "not" ~> ConstraintStatement ^^ (cs =>
      ??? //new Negation(cs)
    )
    
  // TODO 
  def MemberOfList: Parser[Env => Env] =
    Reference ~ "in" ~ Vector <~ eos ^^ { case r ~ _ ~ vec =>
      ??? //new MemberOfList(r, vec)
    }
    
  // TODO 
  def Action: Parser[Env => Env] =
    "(" ~> Parameters ~ ")" ~ "{" ~ Cost ~ "condition" ~ Condition ~ "effect" ~ Effects <~ "}" ^^ {
      case ps ~ _ ~ _ ~ cost ~ _ ~ cond ~ _ ~ eff =>
        ??? //new Action(ps, cost, cond, eff)
    }
  
  // TODO 
  def Parameters: Parser[Env => Env] =
    ( Parameter ~ ("," ~> Parameter).* ^^ { case p ~ ps => ??? } //p :: ps }
    | epsilon ^^ (x => ???) //List())
    )
    
  // TODO 
  def Parameter: Parser[Env => Env] =
    ident ~ ":" ~ Type ^^ { case id ~ _ ~ t => ??? } //new Parameter(id, t) }
    
  // TODO 
  def Cost: Parser[Integer] =
    ( "cost" ~> "=" ~> Number <~ eos ^^ (n => ???
        /*if (n.isInstanceOf[Integer]) n.asInstanceOf[Integer]
        else throw new SemanticsException("[err301]", 301)*/
      )
    | epsilon ^^ (x => 1)
    )
    
  // TODO 
  def Condition: Parser[Env => Env] =
    ( Conjunction
    | epsilon ^^ (x => ???) //True)
    )
    
  // TODO 
  def Effects: Parser[Env => Env] =
    "{" ~> Effect.+ <~ "}" ^^ (effs => {
        /*val eff1 = effs.head(null)
        effs.tail.foldRight[Effect](eff1)(
          (ef: Effect => Effect, effs: Effect) => ef(effs)
        )*/
        ???
      }
    )
    
  // TODO 
  def Effect: Parser[Env => Env] =
    Reference ~ "=" ~ BasicValue <~ eos ^^ { case r ~ _ ~ bv =>
      ??? //(eff: Effect) => new SimpleEffect(r, bv, eff)
    }
    
  //--- helper functions ---//
  val epsilon: Parser[Any] = ""

  val eos: Parser[Any] = ";" | '\n'
  
  def parse(filepath: String, s: String): Env = {
    parseAll(Sfp, s) match {
      case Success(root, _) => root
      case NoSuccess(msg, next) => throw new SyntaxError(filepath, next.pos)
    }
  }
    
  def parseIncludeFileAtRoot(filePath: String, e: Env): Env = {
    parseAll(SfpContext, Source.fromFile(filePath).mkString) match {
      case Success(sfp, _) => sfp(e)
      case NoSuccess(msg, next) => throw new SyntaxError(filePath, next.pos)
    }
  }
  
  def parseIncludeFileInBlock(filePath: String, ns: Reference, e: Env): Env = {
    parseAll(Block, Source.fromFile(filePath).mkString) match {
      case Success(b, _) => b(ns, e)
      case NoSuccess(msg, next) => throw new SyntaxError(filePath, next.pos)
    }
  }
    
  def parseFile(filePath: String): Env = parse(filePath, Source.fromFile(filePath).mkString)
}