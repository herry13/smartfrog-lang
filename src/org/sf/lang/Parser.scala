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
      case se: SemanticsException => println(se.msg)
      case e: Exception => println(e)
    }
  }
    
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
        else throw new SemanticsException("[err7] sfConfig is not exist or a component")
      )(b(org.sf.lang.Reference.Empty, Store.Empty).find(sfConfig))
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
          	else throw new SemanticsException("[err6] prefix of " + r + " is not a component")
          )(s.resolve(ns, r.prefix))
    }

  def Prototypes: Parser[(Reference, Reference, Store) => Store] =
    ( Prototype ~ ("," ~> Prototypes).? ^^ { case p ~ ps =>
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
    | "{" ~> Body <~ "}" ^^ { case b =>
        (ns: Reference, r: Reference, s: Store) => b(r, s)
      }
    )
  
  def Value: Parser[(Reference, Reference, Store) => Store] =
    ( BasicValue ~ Ordering <~ ";" ^^ { case sv ~ b =>
        (ns: Reference, r: Reference, s: Store) => b(s)(r, sv)
      }
    | LinkReference <~ ";"  ^^ (lr =>
        (ns: Reference, r: Reference, s: Store) =>
          ((l: (Reference, Any)) =>
            if (l._2 == Store.Undefined) throw new SemanticsException("[err5] cannot find link reference " + lr)
            else s.bind(r, l._2)
          )(s.resolve(ns, lr))
      )
    | "extends" ~> Prototypes ^^ (p =>
        (ns: Reference, r: Reference, s: Store) => p(ns, r, s.bind(r, Store.Empty))
      )
    ) 

  def Ordering: Parser[Store => (Reference, Any) => Store] =
    ( "before" ~> ident ^^ { id => (s: Store) => s.bindBefore(id)_ }
    | "after" ~> ident ^^ { id => (s: Store) => s.bindAfter(id)_ }
    | epsilon ^^ { id => (s: Store) => s.bind }
    )
    
  def Reference: Parser[Reference] =
    ident ~ (":" ~> ident).* ^^ {
      case id ~ ids => new Reference(id, org.sf.lang.Reference(ids))
    }
	
  def DataReference: Parser[Reference] =
    "DATA" ~> Reference

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