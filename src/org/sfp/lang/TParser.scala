package org.sfp.lang

import scala.io.Source

import org.sf.lang.Reference
import org.sf.lang.SemanticsException

object TParser extends TParser with App {
  val help = """Usage: scala org.sfp.lang.TParser [option] <sfp-file>

where [option] is:
  -json     print output in JSON
  -yaml     print output in YAML
"""
    
  if (args.length <= 0) Console.println(help)
  else {
    try {
      if (inJson(args)) ??? //println(parseFile(args.tail.head).toJson)
      else if (inYaml(args)) ??? //println(parseFile(args.tail.head).toYaml)
      else println(parseFile(args.head))
    } catch {
      case se: TypeError => System.err.println(se.msg)
      case e: Exception => System.err.println(e)
    }
  }
    
  def inJson(args: Array[String]): Boolean =
    (args.length >= 2 && args.head.equals("-json"))
    
  def inYaml(args: Array[String]): Boolean =
    (args.length >= 2 && args.head.equals("-yaml"))
}

class TParser extends CommonParser {
  import Type._
  
  def Sfp: Parser[Environment] =
    Statement ^^ (st => st(org.sf.lang.Reference.empty, Type.init))
  
  def Statement: Parser[(Reference, Environment) => Environment] =
    ( _schema ~> Schema ~ Statement ^^ { case sc ~ st =>
        (ns: Reference, e: Environment) => st(ns, sc(e))
      }
    | _global ~> GlobalConstraint ~ Statement ^^ { case g ~ st =>
        (ns: Reference, e: Environment) => st(ns, set(e, global, g))
      }
    | _import ~> stringLiteral ~ eos ~ Statement ^^ { case file ~ _ ~ st =>
      	(ns: Reference, e: Environment) => st(ns, parseImportFile(file.substring(1, file.length-1), ns, e))
      }
    | Assignment ~ Statement ^^ { case ass ~ st =>
        (ns: Reference, e: Environment) => st(ns, ass(ns, e))
      }
    | _epsilon ^^ (x => (ns: Reference, e: Environment) => e)
    )

  // TODO
  def Schema: Parser[Environment => Environment] =
    ident ~ (_extends ~> ParentSchema).? ~ _begin ~ Body <~ _end ^^ {
      case id ~ par ~ _ ~ b =>
        (e: Environment) => {
          val r = new Reference(id)
          if (par.isEmpty) ??? // b(r, define(e, id))
          else ???
        }
    }
  
  // TODO
  def ParentSchema: Parser[(Reference, Environment) => Environment] =
    ident ^^ { case id =>
      val pr = new Reference(id)
      (r: Reference, e: Environment) => ???
    }
  
  def Body: Parser[(Reference, Environment) => Environment] =
    ( _include ~> stringLiteral ~ eos ~ Body ^^ { case file ~ _ ~ b =>
      	(ns: Reference, e: Environment) => b(ns, parseIncludeFile(file, ns, e))
      }
    | Assignment ~ Body ^^ { case a ~ b =>
        (ns: Reference, e: Environment) => b(ns, a(ns, e))
      }
    | _epsilon ^^ { x => (ns: Reference, e: Environment) => e }
    )
    
  // TODO
  def Assignment: Parser[(Reference, Environment) => Environment] =
    ( _action ~> Reference ~ Action
    | Reference ~ SfpValue <~ eos.*
    ) ^^ { case r ~ v =>
      (ns: Reference, e: Environment) => ???
    }

  def Prototypes: Parser[(Reference, Reference, Environment) => Environment] =
    ( Prototype ~ ("," ~> Prototypes).? ^^ { case p ~ ps =>
        (ns: Reference, r: Reference, e: Environment) =>
          if (ps.isEmpty) p(ns, r, e)
          else ps.get(ns, r, p(ns, r, e))
      }
    | _epsilon ^^ { x => (ns: Reference, r: Reference, e: Environment) => e }
    )
  
  // TODO
  def Prototype: Parser[(Reference, Reference, Environment) => Environment] =
    ( Reference ^^ { case r1 =>
        (ns: Reference, r: Reference, e: Environment) => ???
      }
    | _begin ~> Body <~ _end ^^ { case b =>
        (ns: Reference, r: Reference, e: Environment) => b(r, e)
      }
    )
    
  // TODO
  def SfpValue: Parser[(Reference, Reference, Environment) => Environment] =
    ( eq ~> BasicValue <~ eos ^^ (v =>
        (ns: Reference, r: Reference, e: Environment) => set(e, r, v)
      )
    | LinkReference <~ eos ^^ (lr =>
        (ns: Reference, r: Reference, e: Environment) => ???
      )
    | (_isa ~> ParentSchema).? ~ _extends ~ Prototypes ^^ { case sc ~ _ ~ p =>
        (ns: Reference, r: Reference, e: Environment) => ???
      }
    | ":" ~> _Type ^^ (t =>
        (ns: Reference, r: Reference, e: Environment) => ???
      )
    )
  
  def _Type: Parser[T] =
    ( "bool"              ^^ (x => _Tbool)                     // a boolean
    | "[bool]"            ^^ (x => new T("bool", false, true)) // a list of boolean
    | "num"               ^^ (x => _Tnum)                      // a number
    | "[num]"             ^^ (x => new T("num", false, true))  // a list of number
    | "str"               ^^ (x => _Tstr)                      // a string
    | "[str]"             ^^ (x => new T("str", false, true))  // a list of string
    | ident               ^^ (x => new T(x, true, false))      // a reference of particular schema
    | "[" ~> ident <~ "]" ^^ (x => new T(x, true, true))       // a list of reference of particular schema: [Service]
    )
    
  def Reference: Parser[Reference] =
    ident ~ (_sep ~> ident).* ^^ {
      case id ~ ids => new Reference(id, org.sf.lang.Reference(ids))
    }

  // TODO -- reference must be resolved first (forward-data-reference?)
  def DataReference: Parser[T] =
    Reference ^^ (x => _Tref)

  def LinkReference: Parser[Reference] =
    Reference
    
  def BasicValue: Parser[T] =
    ( Boolean
    | Number
    | String 
    | DataReference
    | Vector
    | Null
    )

  def Number: Parser[T] =
    floatingPointNumber ^^ (n => _Tnum)

  def Vector: Parser[T] =
    "[" ~>
    ( BasicValue ~ ("," ~> BasicValue).* ^^ { case x ~ xs =>
        val t = new T(x.name, false, true)
        if (xs.forall((t1: T) => t.equals(t1))) t
        else throw new SemanticsException("[err104]")
      }
    | _epsilon ^^ (x => _Tvec)
    ) <~ "]"

  def Null: Parser[T] = _null ^^ (x => _Tnull)
    
  def Boolean: Parser[T] =
    ( _true ^^ (x => _Tbool)
    | _false ^^ (x => _Tbool)
    )
  
  def String: Parser[T] =
    stringLiteral ^^ (x => _Tstr)
    
  //--- constraints ---//
  def GlobalConstraint: Parser[T] =
    _begin ~> Conjunction <~ _end ^^ (c => _Tconstraint)
    
  def Conjunction: Parser[T] =
    CStatement.* ^^ (cs => _Tconstraint)
    
  def CStatement: Parser[T] = Eq | Neq | In | Imply
  
  def Eq: Parser[T] =
    Reference ~ eq ~ BasicValue <~ eos ^^ { case r ~ _ ~ bv => _Tconstraint }
  
  def Neq: Parser[T] =
    Reference ~ neq ~ BasicValue <~ eos ^^ { case r ~ _ ~ bv => _Tconstraint }
  
  def In: Parser[T] =
    Reference ~ _in ~ Vector <~ eos ^^ { case r ~ _ ~ vec => _Tconstraint }
  
  def Imply: Parser[T] =
    "if" ~> _begin ~> Conjunction ~ (_end ~ "then" ~ _begin) ~ Conjunction <~ _end ^^ {
      case premise ~ _ ~ conclusion => _Tconstraint
    } 
  
  //--- action ---//
  def Action: Parser[(Reference, Reference, Environment) => Environment] =
    Parameters ~ _begin ~ Cost ~ Conditions ~ Effects <~ _end ^^ {
      case pars ~ _ ~ c ~ cond ~ eff =>
        (ns: Reference, r: Reference, e: Environment) => set(e, r, _Taction)
    }
    
  def Parameters: Parser[Map[String, Reference]] =
    ( "(" ~> Parameter.+ <~ ")" ^^ (pars => ???
      )
    | _epsilon ^^ (x => Map())
    )
    
  def Parameter: Parser[(String, Reference)] =
    ident ~ ":" ~ ident ^^ { case id ~ _ ~ t => ??? }
  
  def Cost: Parser[Integer] =
    _cost ~> eq ~> """[0-9]+""".r <~ eos ^^ (n => n.toInt)
    
  def Conditions: Parser[T] =
    _condition ~> _begin ~> Conjunction <~ _end
    
  def Effects: Parser[T] =
    _effect ~> _begin ~> Effect.+ <~ _end ^^ (eff => ???)
    
  def Effect: Parser[T] =
    Reference ~ eq ~ BasicValue <~ eos ^^ { case r ~ _ ~ v => ??? }
    
  def parse(s: String): Environment = {
    parseAll(Sfp, s) match {
      case Success(root, _) => root
      case NoSuccess(msg, next) => throw new TypeError("invalid statement at " + next.pos)
    }
  }

  def parseImportFile(filePath: String, ns: Reference, e: Environment): Environment= {
    parseAll(Statement, Source.fromFile(filePath).mkString) match {
      case Success(stmt, _) => stmt(ns, e)
      case NoSuccess(msg, next) => throw new TypeError("invalid statement at " + next.pos + " in included file " + filePath)
    }
  }

  def parseIncludeFile(filePath: String, ns: Reference, e: Environment): Environment = {
    parseAll(Body, Source.fromFile(filePath).mkString) match {
      case Success(body, _) => body(ns, e)
      case NoSuccess(msg, next) => throw new SemanticsException("invalid statement at " + next.pos)
    }
  }
  
  def parseFile(filePath: String): Environment = parse(Source.fromFile(filePath).mkString)
}