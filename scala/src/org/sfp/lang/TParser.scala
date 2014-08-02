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
        
        val e = sc(new Env())
        e.eval(e)
	  }
    )
  
  def SfpContext: Parser[Env => Env] =
    ( "schema" ~> Schema ~ SfpContext ^^ { case s ~ sc =>
        (e: Env) => sc(s(e))
      }
    | "global" ~> Global ~ SfpContext ^^ { case g ~ sc =>
        (e: Env) => {
          val tr = e.get(ref_global)
          val tg = glob  // (Glob)
          val er = if (tr == null) e + (ref_global, tg)
                   else if (tr <= tg) e
                   else throw new TypeError(17, ref_global + " is not global constraints")
          sc(g(er))
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
          val tr = e.get(ref_global)
          val tg = glob  // (Glob)
          val er = if (tr == null) e + (ref_global, tg)
                   else if (tr <= tg) e
                   else throw new TypeError(16, ref_global + " is not global constraints")
          b(ns, g(er))
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
            else if (tr <= ta) e          // (Assign2)
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
          val t_var = e.get(r)
          val t_val = bv(e)
          if (t_var == null) {
            if (t_val == Undefined ||
                (t_val.isInstanceOf[TVec] && t_val.asInstanceOf[TVec].tau == Undefined) ||
                (t_val.isInstanceOf[TRef] && t_val.asInstanceOf[TRef].tau == Undefined))
              throw new TypeError(13, "cannot assign undefined type to undefined variable")
            e + (r, t_val)  // (Assign1,2)
          }
          else if (t_val <= t_var) e         // (Assign3,4)
          else if (t_var.isInstanceOf[TForward] && t_var.asInstanceOf[TForward].plausible(t_val)) {
            t_var.asInstanceOf[TForward].add(t_val)
            e
          }
          else if (t_val.isInstanceOf[TForward] && t_val.asInstanceOf[TForward].plausible(t_var)) {
            val t = new TDataRef()
            t.tlist = t_var :: t_val.asInstanceOf[TForward].tlist
            e + (r, t)
          }
          else throw new TypeError(14, r + ": " + t_val + " !<: " + t_var + "\n" + e)
        }
      )
    | LinkReference <~ eos ^^ (l => // TODO -- forward link reference
        (r: Reference, e: Env) => {
          val t_var = e.get(r)
          val (rl, t_link) = l(r, e)
          if (t_var == null) (e + (r, t_link)).inherit(rl, r)  // (Assign1,2)
          else if (t_link <= t_var) e.inherit(rl, r)           // (Assign3,4)
          else if (t_var.isInstanceOf[TForward] && t_var.asInstanceOf[TForward].plausible(t_link)) {
            t_var.asInstanceOf[TForward].add(t_link)
            e.inherit(rl, r)
          }
          else throw new TypeError(15, r + ": " + t_link + " !<: " + t_var)
        }
      )
    | SuperSchemaO ~ Prototypes ^^ { case ss ~ ps =>
        (r: Reference, e: Env) => {
          val (tss, es) = ss(r, e)
          ps(r, tss, es)
        }
      }
    )

  def Prototypes: Parser[(Reference, T, Env) => Env] =
    ( Prototype ~ ("," ~> Prototype).* <~ eos ^^ { case proto1 ~ protos =>
        (r_var: Reference, t_schema: T, e: Env) => {
          val (t_proto1, e_proto1) = proto1(r_var, t_schema, null, e, true)
          protos.foldRight[Env](e_proto1)(
            (ps: ((Reference, T, T, Env, Boolean) => (T, Env)), es: Env) => {
              val (_, e_proto) = ps(r_var, t_schema, t_proto1, es, false)
              e_proto
            }
          )
        }
      }
    | eos ^^ { x =>
        (r_var: Reference, t_schema: T, e: Env) => {
          val t_var = e.get(r_var)
          if (t_schema == null) {                // (Object1)
            if (t_var == null) e + (r_var, obj)  // (Assign1)
            else if (obj <= t_var) e             // (Assign2)
            else throw new TypeError(44, r_var + ": " + obj + " !<: " + t_var)
          }
          else e  // do nothing - schema has been checked in SuperSchemaO
        }
      }
    )
  
  def Prototype: Parser[(Reference, T, T, Env, Boolean) => (T, Env)] =
    ( "extends".? ~> Reference ^^ { case r_proto =>
        (r_var: Reference, t_schema: T, t_proto1: T, e: Env, firstProto: Boolean) => {
          val t_proto = e.get(r_proto)
          if (!(t_proto <= obj)) throw new TypeError(46, r_var + ": " + r_proto + " is not an object")
          val t_var = e.get(r_var)
          if (firstProto) {
            val es = if (t_schema == null) {                    // (Object3)
                       if (t_var == null) e + (r_var, t_proto)  // (Assign1)
                       else if (t_proto <= t_var) e             // (Assign2)
                       else throw new TypeError(47, r_var + ": " + r_proto + " !<: " + t_var)
                     }
                     else {  // (Object4)
                       if (t_proto <= t_schema) e
                       else throw new TypeError(48, r_var + ": " + r_proto + " !<: " + t_schema)
                     }
            (t_proto, es.inherit(r_proto, r_var))
          }
          else {
            if (t_schema == null && !(t_proto <= t_proto1))       // (Object3)
              throw new TypeError(49, r_var + ": " + t_proto + " !<: " + t_proto1)
            else if (t_schema != null && !(t_proto <= t_schema))  // (Object4)
              throw new TypeError(50, r_var + ": " + t_proto + " !<: " + t_schema)
            (t_proto, e.inherit(r_proto, r_var))
          }
        }
      }
    | "{" ~> Block <~ "}" ^^ { case block =>
        (r_var: Reference, t_schema: T, t_proto1: T, e: Env, firstProto: Boolean) => {
          val t_var = e.get(r_var)
          val es = if (firstProto && t_schema == null) {  // (Object2)
                     if (t_var == null) e + (r_var, obj)
                     else if (obj <= t_var) e
                     else throw new TypeError(45, r_var + ": " + obj + " !<: " + t_var)
                   }
                   else e  // do nothing - schema has been checked in SuperSchemaO
          val t_proto = if (firstProto) obj else null
          (t_proto, block(r_var, es)) // expand the block
        }
      }
    )
    
  def SuperSchemaO: Parser[(Reference, Env) => (T, Env)] =
    ( "isa" ~> ident ^^ (id_schema =>
        (r_var: Reference, e: Env) => {
          val r_schema = new Reference(id_schema)  // reference of schema
          val t_schema = e.get(r_schema)           // TSchema
          if (t_schema == null) throw new TypeError(41, "schema " + id_schema + " is not exist")
          else if (!t_schema.isInstanceOf[TSchema]) throw new TypeError(42, id_schema + " is not a schema")
          else {
            val t_var = e.get(r_var)  // type of variable
            val tau_schema = t_schema.asInstanceOf[TSchema].tau
            val es = if (t_var == null) e + (r_var, tau_schema)   // (Object4, 5) & (Assign1)
                     else if (tau_schema <= t_var) e              // (Object4, 5) & (Assign2)
                     else throw new TypeError(43, r_var + ": schema " + tau_schema + " !<: " + t_var)
            (tau_schema, es.inherit(r_schema, r_var))  // inherit schema
          }
        }
      )
    | epsilon ^^ (x => (r: Reference, e: Env) => (null, e))  // Object1,2,3)
    )
      
  def Reference: Parser[Reference] =
    ident ~ ("." ~> ident).* ^^ {
      case id ~ ids => new Reference(id, org.sf.lang.Reference(ids))
    }
	
  def DataReference: Parser[Env => T] =
    Reference ^^ (r =>
      (e: Env) => {
        val t_ref = e.get(r)
        if (t_ref == null) {
          val t = new TDataRef()
          t.add(r)
          t
        }
        else new TRef(t_ref)
      }
    )

  def LinkReference: Parser[(Reference, Env) => (Reference, T)] =
    Reference ^^ (rl =>
      (r: Reference, e: Env) =>
        if (rl.subseteqof(r)) throw new SemanticsException("[err4]", 4)
        else {
          val t_ref = e.get(rl)
          if (t_ref == null) {
            val t = new TLinkRef()
            t.add(rl)
            (rl, t)
          }
          else (rl, t_ref)
        }
    )
    
  /**
   * Error codes: 2x
   */
  def Vector: Parser[Env => T] =
    "[" ~>
    ( BasicValue ~ ("," ~> BasicValue).* ^^ { case x ~ xs =>
        (e: Env) => {
          // (Vec)
          val t = x(e)
          if (xs.length > 0) xs.forall(
            (x: (Env => T)) => {
              val tx = x(e)
              if (tx.equals(t)) true
              else throw new TypeError(21, "vec - " + t + "!=" + tx)
            }
          )
          new TVec(t)
        }
      }
    | epsilon ^^ (x => (e: Env) => new TVec(Undefined))
    ) <~ "]"

  def Number: Parser[Any] =
    floatingPointNumber

  def Null: Parser[Any] = "null"
  
  def Boolean: Parser[Any] =
    ( "true"
    | "false"
    )
  
  def BasicValue: Parser[Env => T] =
    ( Boolean       ^^ (x => (e: Env) => bool)   // (Bool)
    | Number        ^^ (x => (e: Env) => num)    // (Num)
    | stringLiteral ^^ (x => (e: Env) => str)    // (Str)
    | Null          ^^ (x => (e: Env) => _null)  // (Null)
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
    ( "[]" ~> tau ^^ (t => new TVec(t))
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
    Conjunction ^^ (gc => ???)
  
  // TODO 
  def Conjunction: Parser[Env => Env] =
    "{" ~>
    ( ConstraintStatement.+ ^^ (cs => ???)
    | epsilon ^^ (cs => ???)
    ) <~ "}"
    
  // TODO 
  def Disjunction: Parser[Env => Env] =
    "(" ~>
    ( ConstraintStatement.+ ^^ (cs => ???)
    | epsilon ^^ (cs => ???)
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
    Reference ~ "=" ~ BasicValue <~ eos ^^ { case r ~ _ ~ bv => ??? }
  
  // TODO 
  def NotEqual: Parser[Env => Env] =
    Reference ~ "!=" ~ BasicValue <~ eos ^^ { case r ~ _ ~ bv => ??? }
  
  // TODO 
  def Implication: Parser[Env => Env] = 
    "if" ~> Conjunction ~ "then" ~ Conjunction ^^ { case premise ~ _ ~ conclusion => ??? }
  
  // TODO 
  def Negation: Parser[Env => Env] =
    "not" ~> ConstraintStatement ^^ (cs => ??? )
    
  // TODO 
  def MemberOfList: Parser[Env => Env] =
    Reference ~ "in" ~ Vector <~ eos ^^ { case r ~ _ ~ vec => ??? }
    
  // TODO 
  def Action: Parser[Env => Env] =
    "(" ~> Parameters ~ ")" ~ "{" ~ Cost ~ "condition" ~ Condition ~ "effect" ~ Effects <~ "}" ^^ {
      case ps ~ _ ~ _ ~ cost ~ _ ~ cond ~ _ ~ eff => ???
    }
  
  // TODO 
  def Parameters: Parser[Env => Env] =
    ( Parameter ~ ("," ~> Parameter).* ^^ { case p ~ ps => ??? }
    | epsilon ^^ (x => ???)
    )
    
  // TODO 
  def Parameter: Parser[Env => Env] =
    ident ~ ":" ~ Type ^^ { case id ~ _ ~ t => ??? }
    
  // TODO 
  def Cost: Parser[Integer] =
    ( "cost" ~> "=" ~> Number <~ eos ^^ (n => ???)
    | epsilon ^^ (x => 1)
    )
    
  // TODO 
  def Condition: Parser[Env => Env] =
    ( Conjunction
    | epsilon ^^ (x => ???)
    )
    
  // TODO 
  def Effects: Parser[Env => Env] =
    "{" ~> Effect.+ <~ "}" ^^ (effs => { ??? })
    
  // TODO 
  def Effect: Parser[Env => Env] =
    Reference ~ "=" ~ BasicValue <~ eos ^^ { case r ~ _ ~ bv => ??? }
    
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