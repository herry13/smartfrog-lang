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
  else
    if (inJson(args)) println(parseFile(args.tail.head).toJson)
    else if (inYaml(args)) println(parseFile(args.tail.head).toYaml)
    else println(parseFile(args.head))
    
  def inJson(args: Array[String]): Boolean =
    (args.length >= 2 && args.head.equals("-json"))
    
  def inYaml(args: Array[String]): Boolean =
    (args.length >= 2 && args.head.equals("-yaml"))
}

class Parser extends JavaTokenParsers {
  protected override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r
  protected val sfConfig = new Reference("sfConfig")

  def Sf: Parser[Store] = Body ^^ (b =>
      ((v: Any) =>
        if (Store.isStore(v)) v.asInstanceOf[Store]
        else throw new Exception("sfConfig is not exist or a component")
      )(
        b(org.sf.lang.Reference.Empty, Store.Empty).find(sfConfig)
      ).accept(Store.replaceLink)
    )

  def Body: Parser[(Reference, Store) => Store] =
    ( Assignment ~ Body ^^ { case a ~ b =>
        (ns: Reference, s: Store) => b(ns, a(ns, s))
      }
    | "#include" ~> stringLiteral ~ ";" ~ Body ^^ { case file ~ _ ~ b =>
      	(ns: Reference, s: Store) => b(ns, parseIncludeFile(file.substring(1, file.length-1), ns, s))
      }
    | epsilon ^^ { x => (ns: Reference, s: Store) => s }
    )
    
  def Assignment: Parser[(Reference, Store) => Store] =
    Reference ~ Value ^^ { case r ~ v =>
      (ns: Reference, s: Store) =>
        if (r.length == 1) v(ns, ns ++ r, s)
        else
          ((l: (Reference, Any)) =>
          	if (Store.isStore(l._2)) v(ns, l._1 ++ r, s)
          	else throw new Exception("prefix of " + r + " is not a component")
          )(s.resolve(ns, r.prefix))
    }

  def Prototype: Parser[(Reference, Reference, Store) => Store] =
    ( "extends" ~> Reference ~ Prototype ^^ { case r1 ~ p =>
        (ns: Reference, r: Reference, s: Store) =>
          p(ns, r, s.inherit(ns, r1, r))
      }
    | "extends" ~> "{" ~> Body ~ "}" ~ Prototype ^^ { case b ~ _ ~ p =>
        (ns: Reference, r: Reference, s: Store) =>
          p(ns, r, b(r, s))
      }
    | epsilon ^^ { x =>
        (ns: Reference, r: Reference, s: Store) => s
      }
    )
  
  def Value: Parser[(Reference, Reference, Store) => Store] =
    ( BasicValue <~ ";"  ^^ (sv =>
        (ns: Reference, r: Reference, s: Store) => s.bind(r, sv)
      )
    | LinkReference <~ ";"  ^^ (lr =>
        (ns: Reference, r: Reference, s: Store) => s.bind(r, lr(r))
      )
    | Prototype ^^ (p =>
        (ns: Reference, r: Reference, s: Store) => p(ns, r, s.bind(r, Store.Empty))
      )
    )
	
  def Reference: Parser[Reference] =
    ident ~ (":" ~> ident).* ^^ {
      case id ~ ids => new Reference(id, org.sf.lang.Reference(ids))
    }
	
  def DataReference: Parser[Reference] =
    "DATA" ~> Reference

  def LinkReference: Parser[Reference => LinkReference] =
    Reference ^^ { case lr =>
      (r: Reference) =>
      	if (lr.subseteqof(r)) throw new Exception("link reference is referring the parent")
      	else new LinkReference(lr)
    }
    
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
    "[" ~>
    ( BasicValue ~ ("," ~> BasicValue).* ^^ { case x ~ xs => x :: xs }
    | epsilon ^^ (x => List())
    ) <~ "]"

  def Null: Parser[Any] = "NULL" ^^ (x => null)
    
  def Boolean: Parser[Boolean] =
    ( "true" ^^ (x => true)
    | "false" ^^ (x => false)
    )
	
  val epsilon: Parser[Any] = ""
  
  def parse(s: String): Store = {
    parseAll(Sf, s) match {
      case Success(root, _) => root
      case NoSuccess(msg, next) => throw new Exception("at " + next.pos)
    }
  }
    
  def parseIncludeFile(filePath: String, ns: Reference, s: Store): Store = {
    parseAll(Body, Source.fromFile(filePath).mkString) match {
      case Success(body, _) => body(ns, s)
      case NoSuccess(msg, next) => throw new Exception("at " + next.pos)
    }
  }
    
  def parseFile(filePath: String): Store = parse(Source.fromFile(filePath).mkString)
}