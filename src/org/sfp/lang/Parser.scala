package org.sfp.lang

import scala.io.Source
import org.sf.lang.Reference
import org.sf.lang.Store
import org.sf.lang.SemanticsException

object Parser extends Parser with App {
  val help = """Usage: scala org.sf.lang.SfpParser [option] <sfp-file>

where [option] is:
  -json     print output in JSON
  -yaml     print output in YAML
"""
    
  if (args.length <= 0) Console.println(help)
  else {
    try {
      if (inJson(args)) println(parseFile(args.tail.head).toJson)
      else if (inYaml(args)) println(parseFile(args.tail.head).toYaml)
      else println(parseFile(args.head))
    } catch {
      case se: SemanticsException => System.err.println(se.msg)
      case e: Exception => System.err.println(e)
    }
  }
    
  def inJson(args: Array[String]): Boolean =
    (args.length >= 2 && args.head.equals("-json"))
    
  def inYaml(args: Array[String]): Boolean =
    (args.length >= 2 && args.head.equals("-yaml"))
}

class Parser extends org.sf.lang.Parser {
  val _main = org.sf.lang.Reference("main")
  
  def Sfp: Parser[Store] =
    Statement ^^ (st => {
        /*val m = st(org.sf.lang.Reference.Empty, Store.Empty).find(_main)
        if (Store.isStore(m)) m.asInstanceOf[Store]
        else throw new SemanticsException("[err101] main is not exist or a component")*/
        st(org.sf.lang.Reference.Empty, Store.Empty)
      }
    )
  
  def Statement: Parser[(Reference, Store) => Store] =
    ( _schema ~> Schema ~ Statement ^^ { case sc ~ st =>
        (ns: Reference, s: Store) => st(ns, sc(s))
      }
    | _global ~> GlobalConstraint ~ Statement ^^ { case g ~ st =>
        (ns: Reference, s: Store) => st(ns, g(s))
      }
    | _import ~> stringLiteral ~ eos ~ Statement ^^ { case file ~ _ ~ st =>
      	(ns: Reference, s: Store) => st(ns, parseImportFile(file.substring(1, file.length-1), ns, s))
      }
    | Assignment ~ Statement ^^ { case ass ~ st =>
        (ns: Reference, s: Store) => st(ns, ass(ns, s))
      }
    | epsilon ^^ (x => (ns: Reference, s: Store) => s)
    )
  
  def Schema: Parser[Store => Store] =
    ident ~ (_extends ~> ParentSchema).? ~ "{" ~ Body <~ "}" ^^ { case id ~ par ~ _ ~ b => {
        val r = new Reference(id)
        (s: Store) =>
          if (par.isEmpty) b(r, s.bind(r, Store.Empty))
          else b(r, par.get(r, s.bind(r, Store.Empty)))
      }
    }
  
  def ParentSchema: Parser[(Reference, Store) => Store] =
    ident ^^ { case id =>
      val pr = new Reference(id)
      (r: Reference, s: Store) => s.inherit(r, pr, r)
    }
  
  override def Assignment: Parser[(Reference, Store) => Store] =
    ( _action ~> Reference ~ Action
    | Reference ~ SfpValue <~ eos.*
    ) ^^ { case r ~ v =>
      (ns: Reference, s: Store) =>
        if (r.length == 1) v(ns, ns ++ r, s)
        else {
          val l = s.resolve(ns, r.prefix)
          if (Store.isStore(l._2)) v(ns, l._1 ++ r, s)
          else throw new SemanticsException("[err102] prefix of " + r + " is not a component")
        }
    }
  
  def SfpValue: Parser[(Reference, Reference, Store) => Store] =
    ( "=" ~> BasicValue <~ eos ^^ (v =>
        (ns: Reference, r: Reference, s: Store) => s.bind(r, v)
      )
    | LinkReference <~ eos ^^ (lr =>
        (ns: Reference, r: Reference, s: Store) =>
          ((l: (Reference, Any)) =>
            if (l._2 == Store.Undefined) throw new SemanticsException("[err103] cannot find link reference " + lr)
            else s.bind(r, l._2)
          )(s.resolve(ns, lr))
      )
    | ("isa" ~> ParentSchema).? ~ _extends ~ Prototypes ^^ { case sc ~ _ ~ p =>
        (ns: Reference, r: Reference, s: Store) =>
          if (sc.isEmpty) p(ns, r, s.bind(r, Store.Empty))
          else p(ns, r, sc.get(r, s.bind(r, Store.Empty)))
      }
    | ":" ~> Type ^^ (t =>
        (ns: Reference, r: Reference, s: Store) => s.bind(r, t)
      )
    )
  
  def Type: Parser[Any] =
    ( ident ^^ (id =>
        if (id.equals("bool")) false
        else if (id.equals("num")) 0
        else if (id.equals("str")) ""
        else Store.Empty
      )
    | TypeVector
    | TypeReference
    )
    
  def TypeReference: Parser[Any] =
    ( ident
    | TypeVector
    ) ^^ (x => null)
    
  def TypeVector: Parser[List[Any]] =
    ident ^^ (id => List())

  override def DataReference: Parser[Reference] =
    Reference

  override def Reference: Parser[Reference] =
    ident ~ ("." ~> ident).* ^^ {
      case id ~ ids => new Reference(id, org.sf.lang.Reference(ids))
    }

  //--- constraints ---//
  val global = new Reference("global")
  def GlobalConstraint: Parser[Store => Store] =
    "{" ~> Conjunction <~ "}" ^^ (c =>
      (s: Store) => {
        val g = s.find(global)
        if (g.isInstanceOf[Expression]) s.bind(global, c.addParams(g.asInstanceOf[Expression].params))
        else s.bind(global, c)
      }
    )
    
  def Conjunction: Parser[Expression] =
    CStatement.* ^^ (cs => new Expression(Expression.and, cs))
    
  def CStatement: Parser[Any] = Eq | Neq | In | Imply
  
  def Eq: Parser[Any] =
    Reference ~ eq ~ BasicValue <~ eos ^^ { case r ~ _ ~ bv => new PairExpression(Expression.eq, r, bv) }
  
  def Neq: Parser[Any] =
    Reference ~ neq ~ BasicValue <~ eos ^^ { case r ~ _ ~ bv => new PairExpression(Expression.neq, r, bv) }
  
  def In: Parser[Any] =
    Reference ~ "in" ~ Vector <~ eos ^^ { case r ~ _ ~ vec => new PairExpression(Expression.in, r, vec) }
  
  def Imply: Parser[Any] =
    "if" ~> "{" ~> Conjunction ~ ("}" ~ "then" ~ "{") ~ Conjunction <~ "}" ^^ {
      case premise ~ _ ~ conclusion => new Expression(Expression.imply, List(premise, conclusion))
    } 
  
  //--- action ---//
  def Action: Parser[(Reference, Reference, Store) => Store] =
    Parameters ~ "{" ~ Cost ~ Conditions ~ Effects <~ "}" ^^ {
      case pars ~ _ ~ c ~ cond ~ eff =>
        (ns: Reference, r: Reference, s: Store) => s.bind(r, new Action(pars, c, cond, eff))
    }
    
  def Parameters: Parser[Map[String, Reference]] =
    ( "(" ~> Parameter.+ <~ ")" ^^ (pars =>
        pars.foldRight[Map[String, Reference]](Map())(
          (p: (String, Reference), m: Map[String, Reference]) => m + p
        )
      )
    | epsilon ^^ (x => Map())
    )
    
  def Parameter: Parser[(String, Reference)] =
    ident ~ ":" ~ ident ^^ { case id ~ _ ~ t => (id, new Reference(t)) }
  
  def Cost: Parser[Integer] =
    "cost" ~> eq ~> """[0-9]+""".r <~ eos ^^ (n => n.toInt)
    
  def Conditions: Parser[Expression] =
    "condition" ~> "s".? ~> "{" ~> Conjunction <~ "}"
    
  def Effects: Parser[Expression] =
    "effect" ~> "s".? ~> "{" ~> Effect.+ <~ "}" ^^ (eff => new Expression(Expression.and, eff))
    
  def Effect: Parser[Expression] =
    Reference ~ eq ~ BasicValue <~ eos ^^ { case r ~ _ ~ v => new PairExpression(Expression.eq, r, v) }
    
  //--- helpers ---//
  val eq = "=" | "is"
  val neq = "!=" | "isnt"
  override val include: Parser[Any] = "include" | "#include"
  override val _true: Parser[Any] = "true" | "yes"
  override val _false: Parser[Any] = "false" | "no"
  val _schema: Parser[Any] = "schema"
  val _global: Parser[Any] = "global"
  val _import: Parser[Any] = "import"
  val _isa: Parser[Any] = "isa"
  val _action: Parser[Any] = "def" | "sub" | "action"

  override def parse(s: String): Store = {
    parseAll(Sfp, s) match {
      case Success(root, _) => root
      case NoSuccess(msg, next) => throw new SemanticsException("invalid statement at " + next.pos)
    }
  }

  def parseImportFile(filePath: String, ns: Reference, s: Store): Store = {
    parseAll(Statement, Source.fromFile(filePath).mkString) match {
      case Success(stmt, _) => stmt(ns, s)
      case NoSuccess(msg, next) => throw new SemanticsException("invalid statement at " + next.pos + " in included file " + filePath)
    }
  }
}