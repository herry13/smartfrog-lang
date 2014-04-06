package org.sf.lang

import scala.util.parsing.combinator.JavaTokenParsers
import scala.io.Source

object Parser extends Parser with App {
  val help = """Usage: scala org.sf.lang.Parser [option] <sf-file>

where [option] is:
  -json     print output in JSON
  -yaml     print output in YAML
"""

  /*  
  if (args.length <= 0) Console.println(help)
  else
    if (inJson(args)) println(parseFile(args.tail.head).toJson)
    else if (inYaml(args)) println(parseFile(args.tail.head).toYaml)
    else println(parseFile(args.head))
    
  def inJson(args: Array[String]): Boolean =
    (args.length >= 2 && args.head.equals("-json"))
    
  def inYaml(args: Array[String]): Boolean =
    (args.length >= 2 && args.head.equals("-yaml"))
  */
}

class Parser extends JavaTokenParsers {
  import Domain._
  
  protected override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r
  protected val sfConfig = List("sfConfig")
  
  def Sf: Parser[Store] = Body ^^ (b =>
      ((v: Any) =>
        if (v.isInstanceOf[Store]) replaceLink(v.asInstanceOf[Store])
        else throw new Exception("sfConfig is not exist or a component")
      )(
        find(b(List(), Empty), sfConfig)
      )
    )

  def Body: Parser[(Reference, Store) => Store] = AttributeList
  
  def AttributeList: Parser[(Reference, Store) => Store] =
    ( Attribute ~ AttributeList ^^ { case a ~ al =>
        (ns: Reference, s: Store) => al(ns, a(ns, s))
      }
    | "#include" ~> stringLiteral ~ AttributeList ^^ { case file ~ al =>
        (ns: Reference, s: Store) => al(ns, parseIncludeFile(file, ns, s))
      }
    | ";" ~> AttributeList
    | (epsilon | "\\Z".r) ^^ (x =>
        (ns: Reference, s: Store) => s
      )
    )

  def Attribute: Parser[(Reference, Store) => Store] =
    Name ~ Value ^^ { case name ~ value =>
      (ns: Reference, s: Store) =>
        if (name.length == 1) value(ns, ns ++ name, s)
        else
          ((l: (Reference, Any)) =>
          	if (l._2.isInstanceOf[Store]) value(ns, l._1 ++ name, s)
          	else throw new Exception("prefix of " + name + " is not a component")
          )(resolve(s, ns, prefix(name)))
    }
	
  def Name: Parser[Reference] =
    ( "--" ^^ (x =>
        List("x" + scala.util.Random.alphanumeric.take(5).mkString)
      )
    | Word ~ (":" ~> Name).? ^^ { case word ~ name =>
      	if (name.isEmpty) List(word)
      	else word :: name.last
      }
    )

  def Word: Parser[String] = ident

  def Value: Parser[(Reference, Reference, Store) => Store] =
    ( Component ^^ (c =>
        (ns: Reference, r: Reference, s: Store) => c(ns, r, bind(s, r, Empty))
      )
    | SimpleValue <~ ";"  ^^ (sv =>
        (ns: Reference, r: Reference, s: Store) => bind(s, r, sv)
      )
    | LinkReference <~ ";"  ^^ (lr =>
        (ns: Reference, r: Reference, s: Store) => bind(s, r, lr(r))
      )
    | ";" ^^ { x => (ns: Reference, r: Reference, s: Store) => s } 
    )
	
  def SimpleValue: Parser[Any] =
    Basic

  def Basic: Parser[Any] =
    ( stringLiteral
    | Number
    | Boolean
    | ReferenceValue
    | BasicVector
    | Null
    )

  val TBD: Parser[Any] = "TBD" ^^ (x => TBD)
    
  protected val intRegex = """\-?[0-9]+(?!\.)""".r
  
  def Number: Parser[Any] =
    floatingPointNumber ^^ (n =>
      if (intRegex.findFirstIn(n).get != n) n.toDouble
      else n.toInt
    )

  def BasicVector: Parser[Vector] =
    "[|" ~>
    ( Basic ~ ("," ~> Basic).* ^^ { case x ~ xs => x :: xs }
    | epsilon ^^ (x => List())
    ) <~ "|]"

  def Null: Parser[Any] = "NULL" ^^ (x => null)
    
  def Vector: Parser[Vector] =
    "[" ~>
    ( SimpleValue ~ ("," ~> SimpleValue).* ^^ { case x ~ xs => x :: xs }
    | epsilon ^^ (x => List())
    ) <~ "]"
    
  def Boolean: Parser[Boolean] =
    ( "true" ^^ (x => true)
    | "false" ^^ (x => false)
    )
	
  def ReferenceValue: Parser[Reference] =
    "DATA" ~> BaseReference

  def BaseReference: Parser[Reference] =
    ReferencePart ~ (":" ~> ReferencePart).* ^^ {
      case r ~ rs => r :: rs
    }
	
  def ReferencePart: Parser[String] =
    ( "ROOT"
    | "PARENT"
    | "ATTRIB"
    | "HERE"
    | "THIS"
    | Word
    )
	
  def LinkReference: Parser[Reference => LinkReference] =
    OptionalValueLinkReference.? ~ BaseReference ^^ { case opt ~ lr =>
      (r: Reference) =>
        if (opt.isEmpty)
          if (subseteq(lr, r)) throw new Exception("link reference is referring the parent")
          else (lr, null)
        else (lr, opt.head)
    }

  def OptionalValueLinkReference: Parser[Any] =
    "OPTIONAL" ~> SimpleValue
        
  def Component: Parser[(Reference, Reference, Store) => Store] =
    "extends" ~> Prototypes

  def Prototypes: Parser[(Reference, Reference, Store) => Store] =
    ( Prototype ~ ("," ~> Prototype).* ^^ { case p ~ ps =>
        (ns: Reference, r: Reference, s: Store) =>
          ps.foldRight(p(ns, r, s))(
            (p1: (Reference, Reference, Store) => Store, s1: Store) =>
              p1(ns, r, s1)
          )
      }
    | epsilon ^^ (x => (ns: Reference, r: Reference, s: Store) => s)
    )
    
  def Prototype: Parser[(Reference, Reference, Store) => Store] =
    ( BaseReference ^^ (br =>
        (ns: Reference, r: Reference, s: Store) => inherit(s, ns, br ,r)
      )
    | "{" ~> AttributeList <~ "}" ^^ (attrs =>
        (ns: Reference, r: Reference, s: Store) => attrs(r, s)
      )
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