package org.sf.lang

import scala.util.parsing.combinator.JavaTokenParsers
import scala.io.Source

object Parser extends Parser with App {
  val help = """Usage: scala org.sf.lang.Parser [option] <sf-file>

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

class Parser extends CommonParser {
  // ignore space, tabs, newline, and C-style comments
  protected override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r
  
  // main component's reference
  protected val refMain = new Reference("sfConfig")

  def Sf: Parser[Store] =
    Body ^^ { b => {
        val v = b(org.sf.lang.Reference.empty, Store.empty).find(refMain)
        if (v.isInstanceOf[Store]) v.asInstanceOf[Store]
        else throw new SemanticsException("[err7] sfConfig is not exist or a component")
      }
    }

  def Body: Parser[(Reference, Store) => Store] =
    ( _include ~> stringLiteral ~ eos ~ Body ^^ { case file ~ _ ~ b =>
      	(ns: Reference, s: Store) => b(ns, parseIncludeFile(file.substring(1, file.length-1), ns, s))
      }
    | Assignment ~ Body ^^ { case a ~ b =>
        (ns: Reference, s: Store) => b(ns, a(ns, s))
      }
    | epsilon ^^ { x => (ns: Reference, s: Store) => s }
    )
    
  def Assignment: Parser[(Reference, Store) => Store] =
    ( _merge ~> Reference ~ Value ^^ { case r ~ v =>
        (ns: Reference, s: Store) =>
          val l = s.resolve(ns, r)
          if (l._2 == Store.undefined) throw new SemanticsException("[err9] cannot merge with an undefined value")
          else v(ns, l._1 ++ r, s)
      }
    | Reference ~ Value ^^ { case r ~ v =>
        (ns: Reference, s: Store) => v(ns, ns ++ r, s)
      }
    )

  def Prototypes: Parser[(Reference, Reference, Store) => Store] =
    ( Prototype ~ (itemSep ~> Prototypes).? ^^ { case p ~ ps =>
        (ns: Reference, r: Reference, s: Store) =>
          if (ps.isEmpty) p(ns, r, s)
          else ps.get(ns, r, p(ns, r, s))
      }
    | epsilon ^^ { x => (ns: Reference, r: Reference, s: Store) => s }
    )
  
  def Prototype: Parser[(Reference, Reference, Store) => Store] =
    ( Reference ^^ { case r1 =>
        (ns: Reference, r: Reference, s: Store) => s.inherit(ns, r1, r)
      }
    | begin ~> Body <~ end ^^ { case b =>
        (ns: Reference, r: Reference, s: Store) => b(r, s)
      }
    )
  
  def Value: Parser[(Reference, Reference, Store) => Store] =
    ( BasicValue <~ eos.+ ^^ (sv =>
        (ns: Reference, r: Reference, s: Store) => s.bind(r, sv)
      )
    | LinkReference <~ eos.+  ^^ (lr =>
        (ns: Reference, r: Reference, s: Store) => {
          val l = s.resolve(ns, lr)
          if (l._2 == Store.undefined) throw new SemanticsException("[err5] cannot find link reference " + lr)
          else s.bind(r, l._2)
        }
      )
    | _extends ~> Prototypes <~ eos.* ^^ (ps =>
        (ns: Reference, r: Reference, s: Store) => ps(ns, r, s.bind(r, Store.empty))
      )
    | begin ~> Body <~ end ^^ { case b => // syntactic sugar
        (ns: Reference, r: Reference, s: Store) => b(r, s.bind(r, Store.empty))
      }
    )
	
  def Reference: Parser[Reference] =
    ident ~ (refSep ~> ident).* ^^ {
      case id ~ ids => new Reference(id, org.sf.lang.Reference(ids))
    }
	
  def DataReference: Parser[Reference] =
    _data ~> Reference

  def LinkReference: Parser[Reference] =
    Reference
    
  def BasicValue: Parser[Any] =
    ( Boolean
    | Number
    | stringLiteral
    | DataReference
    | Vector
    | Null
    )

  protected val intRegex = """\-?[0-9]+(?!\.)""".r
  
  def Number: Parser[Any] =
    floatingPointNumber ^^ (n =>
      if (intRegex.findFirstIn(n).get != n) n.toDouble
      else n.toInt
    )

  def Vector: Parser[List[Any]] =
    vecBegin ~>
    ( BasicValue ~ (itemSep ~> BasicValue).* ^^ { case x ~ xs => x :: xs }
    | epsilon ^^ (x => List())
    ) <~ vecEnd

  def Null: Parser[Any] = _null ^^ (x => null)
    
  def Boolean: Parser[Boolean] =
    ( _true ^^ (x => true)
    | _false ^^ (x => false)
    )
	
  def parse(s: String): Store = {
    parseAll(Sf, s) match {
      case Success(root, _) => root
      case NoSuccess(msg, next) => throw new SemanticsException("invalid statement at " + next.pos)
    }
  }
    
  def parseIncludeFile(filePath: String, ns: Reference, s: Store): Store = {
    parseAll(Body, Source.fromFile(filePath).mkString) match {
      case Success(body, _) => body(ns, s)
      case NoSuccess(msg, next) => throw new SemanticsException("invalid statement at " + next.pos)
    }
  }
    
  def parseFile(filePath: String): Store = parse(Source.fromFile(filePath).mkString)
}

trait CommonParser extends JavaTokenParsers {
   //--- helper functions ---//
  val epsilon: Parser[Any] = ""
  val eos: Parser[Any] = ";"
  val eof: Parser[Any] = "\\Z".r
  val newline: Parser[Any] = """(\r?\n)+""".r
  val begin: Parser[Any] = "{"
  val end: Parser[Any] = "}"
  val itemSep: Parser[Any] = ","
  val vecBegin: Parser[Any] = "["
  val vecEnd: Parser[Any] = "]"
  val refSep: Parser[Any] = ":" | "."
  val _null: Parser[Any] = "NULL" | "null" | "nil"
  val _true: Parser[Any] = "true" | "yes"
  val _false: Parser[Any] = "false" | "no"
  val _include: Parser[Any] = "#include" | "include"
  val _merge: Parser[Any] = "++"
  val _extends: Parser[Any] = "extends" | "<"
  val _data: Parser[Any] = "DATA" | "$."
}
