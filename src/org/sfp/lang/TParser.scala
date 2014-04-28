package org.sfp.lang

import scala.io.Source

import org.sf.lang.Reference

object TParser extends TParser with App {
  val help = """Usage: scala org.sfp.lang.TParser [option] <sfp-file>

where [option] is:
  -json     print output in JSON
  -yaml     print output in YAML
"""
    
  /*if (args.length <= 0) Console.println(help)
  else {
    try {
      if (inJson(args)) println(parseFile(args.tail.head).toJson)
      else if (inYaml(args)) println(parseFile(args.tail.head).toYaml)
      else println(parseFile(args.head))
    } catch {
      case se: TypeError => System.err.println(se.msg)
      case e: Exception => System.err.println(e)
    }
  }*/
    
  def inJson(args: Array[String]): Boolean =
    ??? //(args.length >= 2 && args.head.equals("-json"))
    
  def inYaml(args: Array[String]): Boolean =
    ??? //(args.length >= 2 && args.head.equals("-yaml"))
}

class TParser extends CommonParser {
  import Type._
  
  def Sfp: Parser[Environment] =
    Statement ^^ (st => st(org.sf.lang.Reference.Empty, Type.init))
  
  def Statement: Parser[(Reference, Environment) => Environment] =
    ( _schema ~> Schema ~ Statement ^^ { case sc ~ st =>
        (ns: Reference, e: Environment) => st(ns, sc(e))
      }
    | _global ~> GlobalConstraint ~ Statement ^^ { case g ~ st =>
        (ns: Reference, e: Environment) => ???
      }
    | _import ~> stringLiteral ~ eos ~ Statement ^^ { case file ~ _ ~ st =>
      	(ns: Reference, e: Environment) => st(ns, parseImportFile(file.substring(1, file.length-1), ns, e))
      }
    | Assignment ~ Statement ^^ { case ass ~ st =>
        (ns: Reference, e: Environment) => st(ns, ass(ns, e))
      }
    | epsilon ^^ (x => (ns: Reference, e: Environment) => e)
    )

  def Schema: Parser[Environment => Environment] =
    ident ~ (_extends ~> ParentSchema).? ~ "{" ~ Body <~ "}" ^^ {
      case id ~ par ~ _ ~ b =>
        (e: Environment) => {
          val r = new Reference(id)
          if (par.isEmpty) ??? // b(r, define(e, id))
          else ???
        }
    }
  
  def ParentSchema: Parser[(Reference, Environment) => Environment] =
    ident ^^ { case id =>
      val pr = new Reference(id)
      (r: Reference, e: Environment) => ???
    }
  
  def Body: Parser[(Reference, Environment) => Environment] =
    ( include ~> stringLiteral ~ eos ~ Body ^^ { case file ~ _ ~ b =>
      	(ns: Reference, e: Environment) => ???
      }
    | Assignment ~ Body ^^ { case a ~ b =>
        (ns: Reference, e: Environment) => ???
      }
    | epsilon ^^ { x => (ns: Reference, e: Environment) => e }
    )
    
  def Assignment: Parser[(Reference, Environment) => Environment] =
    ( _action ~> Reference ~ Action
    | Reference ~ SfpValue <~ eos.*
    ) ^^ { case r ~ v =>
      (ns: Reference, e: Environment) => ???
    }

  def Prototypes: Parser[(Reference, Reference, Environment) => Environment] =
    ( Prototype ~ ("," ~> Prototypes).? ^^ { case p ~ ps =>
        (ns: Reference, r: Reference, e: Environment) => ???
      }
    | epsilon ^^ { x => (ns: Reference, r: Reference, e: Environment) => e }
    )
  
  def Prototype: Parser[(Reference, Reference, Environment) => Environment] =
    ( Reference ^^ { case r1 =>
        (ns: Reference, r: Reference, e: Environment) => ???
      }
    | "{" ~> Body <~ "}" ^^ { case b =>
        (ns: Reference, r: Reference, e: Environment) => ???
      }
    )
    
  def SfpValue: Parser[(Reference, Reference, Environment) => Environment] =
    ( "=" ~> BasicValue <~ eos ^^ (v =>
        (ns: Reference, r: Reference, e: Environment) => ???
      )
    | LinkReference <~ eos ^^ (lr =>
        (ns: Reference, r: Reference, e: Environment) => ???
      )
    | ("isa" ~> ParentSchema).? ~ _extends ~ Prototypes ^^ { case sc ~ _ ~ p =>
        (ns: Reference, r: Reference, e: Environment) => ???
      }
    | ":" ~> _Type ^^ (t =>
        (ns: Reference, r: Reference, e: Environment) => ???
      )
    )

  def _Type: Parser[Any] =
    ( ident
    | TypeVector
    | TypeReference
    ) ^^ (id => ???)
    
  def TypeVector: Parser[Any] =
    ident ^^ (id => ???)

  def TypeReference: Parser[Any] =
    ( ident
    | TypeVector
    ) ^^ (x => ???)    
    
  def Reference: Parser[Reference] =
    ident ~ ("." ~> ident).* ^^ {
      case id ~ ids => new Reference(id, org.sf.lang.Reference(ids))
    }

  def DataReference: Parser[T] =
    Reference ^^ (x => ???)

  def LinkReference: Parser[Reference] =
    Reference
    
  def BasicValue: Parser[T] =
    ( Boolean
    | Number
    | stringLiteral
    | DataReference
    | Vector
    | Null
    )

  def Number: Parser[T] =
    floatingPointNumber ^^ (n => _num)

  def Vector: Parser[T] =
    "[" ~>
    ( BasicValue ~ ("," ~> BasicValue).* ^^ { case x ~ xs => _vec }
    | epsilon ^^ (x => _vec)
    ) <~ "]"

  def Null: Parser[T] = _null ^^ (x => _ref)
    
  def Boolean: Parser[T] =
    ( _true ^^ (x => _bool)
    | _false ^^ (x => _bool)
    )
  
  
  //--- constraints ---//
  val global = new Reference("global")
  def GlobalConstraint: Parser[T] =
    "{" ~> Conjunction <~ "}" ^^ (c => _constraint)
    
  def Conjunction: Parser[T] =
    CStatement.* ^^ (cs => _constraint)
    
  def CStatement: Parser[T] = Eq | Neq | In | Imply
  
  def Eq: Parser[T] =
    Reference ~ eq ~ BasicValue <~ eos ^^ { case r ~ _ ~ bv => _constraint }
  
  def Neq: Parser[T] =
    Reference ~ neq ~ BasicValue <~ eos ^^ { case r ~ _ ~ bv => _constraint }
  
  def In: Parser[T] =
    Reference ~ "in" ~ Vector <~ eos ^^ { case r ~ _ ~ vec => _constraint }
  
  def Imply: Parser[T] =
    "if" ~> "{" ~> Conjunction ~ ("}" ~ "then" ~ "{") ~ Conjunction <~ "}" ^^ {
      case premise ~ _ ~ conclusion => _constraint
    } 
  
  //--- action ---//
  def Action: Parser[(Reference, Reference, Environment) => Environment] =
    Parameters ~ "{" ~ Cost ~ Conditions ~ Effects <~ "}" ^^ {
      case pars ~ _ ~ c ~ cond ~ eff =>
        (ns: Reference, r: Reference, e: Environment) => ???
    }
    
  def Parameters: Parser[Map[String, Reference]] =
    ( "(" ~> Parameter.+ <~ ")" ^^ (pars => ???
      )
    | epsilon ^^ (x => Map())
    )
    
  def Parameter: Parser[(String, Reference)] =
    ident ~ ":" ~ ident ^^ { case id ~ _ ~ t => ??? }
  
  def Cost: Parser[Integer] =
    "cost" ~> eq ~> """[0-9]+""".r <~ eos ^^ (n => n.toInt)
    
  def Conditions: Parser[T] =
    "condition" ~> "s".? ~> "{" ~> Conjunction <~ "}"
    
  def Effects: Parser[T] =
    "effect" ~> "s".? ~> "{" ~> Effect.+ <~ "}" ^^ (eff => ???)
    
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
    
  def parseFile(filePath: String): Environment = parse(Source.fromFile(filePath).mkString)
}