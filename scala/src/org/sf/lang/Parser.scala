package org.sf.lang

import scala.util.parsing.combinator.JavaTokenParsers
import scala.io.Source

/**
 * The main object of sfParser application.
 */
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
      else if (inSf(args)) println(parseFile(args.tail.head).toSf)
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
    
  def inSf(args: Array[String]): Boolean =
    (args.length >= 2 && args.head.equals("-sf"))
}

/**
 * A class that parses an SF codes. It implements the semantics valuation functions.
 * 
 * Errors:
 * - [err5] : the link reference is invalid
 * - [err7] : main component (sfConfig) is not exist
 */
class Parser extends JavaTokenParsers {
  protected override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

  def Sf: Parser[Store] =
    Body ^^ { b => {
    	val r = new Reference("sfConfig")
    	val s1 = b(org.sf.lang.Reference.empty, Store.empty)
    	val v1 = s1.find(r)
    	val s2 = if (v1.isInstanceOf[Store]) s1.accept(r, v1.asInstanceOf[Store], r)
    	         else throw new SemanticsException("[err10]", 10)
    	val v2 = s2.find(r)
    	
    	if (v2.isInstanceOf[Store]) v2.asInstanceOf[Store]
    	else throw new SemanticsException("[err10]", 10)
      }
    }

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
      (ns: Reference, s: Store) => v(ns, ns ++ r, s)
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
    ( BasicValue <~ ";"  ^^ (sv =>
        (ns: Reference, r: Reference, s: Store) => s.bind(r, sv)
      )
    | LinkReference <~ ";"  ^^ (lr =>
        (ns: Reference, r: Reference, s: Store) => s.bind(r, lr(r))
      )
    | "extends" ~> Prototypes ^^ (p =>
        (ns: Reference, r: Reference, s: Store) => p(ns, r, s.bind(r, Store.empty))
      )
    )
	
  def Reference: Parser[Reference] =
    ident ~ (":" ~> ident).* ^^ {
      case id ~ ids => new Reference(id, org.sf.lang.Reference(ids))
    }
	
  def DataReference: Parser[Reference] =
    "DATA" ~> Reference

  def LinkReference: Parser[Reference => LinkReference] =
    Reference ^^ (rp =>
      (r: Reference) =>
        if (rp.subseteqof(r)) throw new SemanticsException("[err4]", 4)
        else new LinkReference(rp)
    )
    
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
      case NoSuccess(msg, next) => throw new SemanticsException("[err0] invalid statement at " + next.pos, 0)
    }
  }
    
  def parseIncludeFile(filePath: String, ns: Reference, s: Store): Store = {
    parseAll(Body, Source.fromFile(filePath).mkString) match {
      case Success(body, _) => body(ns, s)
      case NoSuccess(msg, next) => throw new SemanticsException("[err0] invalid statement at " + next.pos, 0)
    }
  }
    
  def parseFile(filePath: String): Store = parse(Source.fromFile(filePath).mkString)
}
