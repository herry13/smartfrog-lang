package org.sf.lang

import scala.util.parsing.combinator.JavaTokenParsers
import scala.io.Source

class Parser extends JavaTokenParsers {
  protected override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r
  
  def Sf: Parser[Store] = Body ^^ (b =>
      ((v: Any) =>
        if (v.isInstanceOf[Store]) v.asInstanceOf[Store]
        else throw new Exception("sfConfig is not exist or a component")
      )(b(Reference.empty)(Store.Empty).accept(Store.replaceLink).find(Reference("sfConfig")))
    )

  def Body: Parser[Reference => Store => Store] = AttributeList
  
  def AttributeList: Parser[Reference => Store => Store] =
    ( Attribute ~ AttributeList ^^ { case a ~ al =>
        (ns: Reference) => (s: Store) => al(ns)(a(ns)(s))
      }
    | "#include" ~> stringLiteral ~ AttributeList ^^ { case file ~ al =>
        (ns: Reference) => (s: Store) =>
          al(ns)(parseIncludeFile(file, ns, s))
      }
    | "#include?" ~> stringLiteral ~ AttributeList ^^ (x => ???) // TODO
    | ";" ~> AttributeList
    | (epsilon | "\\Z".r) ^^ (x => (ns: Reference) => (s: Store) => s)
    )

  def Attribute: Parser[Reference => Store => Store] =
    Name ~ Value ^^ { case name ~ value =>
      (ns: Reference) => (s: Store) =>
        if (name.length == 1) value(ns)(ns ++ name)(s)
        else
          ((l: (Reference, Any)) =>
          	if (l._2.isInstanceOf[Store]) value(ns)(l._1 ++ name)(s)
          	else throw new Exception("prefix of " + name + " is not a component")
          )(s.resolve(ns, name.prefix))
    }
	
  def Name: Parser[Reference] =
    ( "--" ^^ (x => Reference("x" + scala.util.Random.alphanumeric.take(5).mkString))
    | Word ~ (":" ~> Name).? ^^ { case word ~ name =>
      	if (name.isEmpty) Reference(word)
      	else Reference(word, name.last)
      }
    )

  def Word: Parser[String] = ident

  def Value: Parser[Reference => Reference => Store => Store] =
    ( Component ^^ (c =>
        (ns: Reference) => (r: Reference) => (s: Store) => ???
      )
    | SimpleValue <~ ";"  ^^ (sv =>
        (ns: Reference) => (r: Reference) => (s: Store) => s.bind(r, sv)
      )
    | LinkReference <~ ";"  ^^ (lr =>
        (ns: Reference) => (r: Reference) => (s: Store) => s.bind(r, lr(r))
      )
    | ";" ^^ { x => (ns: Reference) => (r: Reference) => (s: Store) => s } 
    )
	
  def SimpleValue: Parser[Any] =
    ( Basic
    | TBD
    | Function
    | Predicate
    )

  def Basic: Parser[Any] =
    ( stringLiteral
    | Number
    | Boolean
    | ByteArray
    | ReferenceValue
    | BasicVector
    | Null
    )

  val TBD: Parser[Any] = "TBD" ^^ (x => Store.TBD)
    
  protected val intRegex = """\-?[0-9]+(?!\.)""".r
  
  def Number: Parser[Any] =
    floatingPointNumber ^^ (n =>
      if (intRegex.findFirstIn(n).get != n) n.toDouble
      else n.toInt
    )

  def BasicVector: Parser[List[Any]] =
    "[|" ~>
    ( Basic ~ ("," ~> Basic).* ^^ { case x ~ xs => x :: xs }
    | epsilon ^^ (x => List())
    ) <~ "|]"

  def Null: Parser[Any] = "NULL" ^^ (x => null)
    
  def Vector: Parser[List[Any]] =
    "[" ~>
    ( SimpleValue ~ ("," ~> SimpleValue).* ^^ { case x ~ xs => x :: xs }
    | epsilon ^^ (x => List())
    ) <~ "]"
    
  def Boolean: Parser[Boolean] =
    ( "true" ^^ (x => true)
    | "false" ^^ (x => false)
    )
	
  // TODO
  def ByteArray: Parser[List[Any]] =
    ( "#HEX#" ~> "([a-z][A-Z][0-9])+".r <~ "#"
    | "#DEC#" ~> "([a-z][A-Z][0-9])+".r <~ "#"
    | "#OCT#" ~> "([a-z][A-Z][0-9])+".r <~ "#"
    | "#BIN#" ~> "([a-z][A-Z][0-9])+".r <~ "#"
    | "#B64#" ~> "([a-z][A-Z][0-9])+".r <~ "#"
    ) ^^ (x => ???)

  def ReferenceValue: Parser[Reference] =
    "DATA" ~> BaseReference

  def BaseReference: Parser[Reference] =
    ReferencePart ~ (":" ~> ReferencePart).* ^^ {
      case r ~ rs => Reference(r, Reference(rs))
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
          if (lr.subseteqof(r)) throw new Exception("link reference is referring the parent")
          else new LinkReference(lr)
        else new LinkReference(lr, opt.head)
    }

  def OptionalValueLinkReference: Parser[Any] =
    "OPTIONAL" ~> SimpleValue
        
  def Component: Parser[Reference => Reference => Store => Store] =
    "extends" ~> Prototypes

  def Prototypes: Parser[Reference => Reference => Store => Store] =
    ( Prototype ~ ("," ~> Prototype).* ^^ { case p ~ ps =>
        (ns: Reference) => (r: Reference) => (s: Store) =>
          ps.foldRight(p(ns)(r)(s))(
            (p1: Reference => Reference => Store => Store, s1: Store) =>
              p1(ns)(r)(s1)
          )
      }
    | epsilon ^^ (x => (ns: Reference) => (r: Reference) => (s: Store) => s)
    )
    
  def Prototype: Parser[Reference => Reference => Store => Store] =
    ( BaseReference ^^ (br =>
        (ns: Reference) => (r: Reference) => (s: Store) => s.inherit(ns, br, r)
      )
    | "{" ~> AttributeList <~ "}" ^^ (attrs =>
        (ns: Reference) => (r: Reference) => (s: Store) => attrs(r)(s)
      )
    )
  
  // TODO
  def Operator: Parser[List[Any]] =
    ( UnaryOp ~ SimpleValue
    | (SimpleValue ~ (BinaryOp ~ SimpleValue)?)
    | (SimpleValue ~ (NaryOp ~ SimpleValue)*)
    ) ^^ (x => ???)
	  
  // TODO
  def UnaryOp: Parser[List[Any]] = "!" ^^ (x => ???)
  
  // TODO
  def BinaryOp: Parser[List[Any]] =
    ("-" | "/" | "==" | "!=" | ">=" | ">" | "<=" | "<") ^^ (x => ???)
	
  // TODO
  def NaryOp: Parser[List[Any]] =
    ("+" | "*" | "++" | "<>" | "&&" | "||") ^^ (x => ???)
	
  // TODO
  def IfThenElse: Parser[List[Any]] =
    "IF" ~> SimpleValue <~ "THEN" ~> SimpleValue <~ "ELSE" ~> SimpleValue <~ "FI" ^^ (x => ???)
	  
  // TODO
  def Function: Parser[List[Any]] = ???

  // TODO
  def Predicate: Parser[List[Any]] = ???

  val epsilon: Parser[Any] = ""
  
  def parse(s: String): Any = {
    parseAll(Sf, s) match {
      case Success(root, _) => root
      case NoSuccess(msg, next) => throw new Exception("at " + next.pos)
    }
  }
    
  def parseIncludeFile(filePath: String, ns: Reference, s: Store): Store = {
    parseAll(Body, Source.fromFile(filePath).mkString) match {
      case Success(body, _) => body(ns)(s)
      case NoSuccess(msg, next) => throw new Exception("at " + next.pos)
    }
  }
    
  def parseFile(filePath: String) = parse(Source.fromFile(filePath).mkString)
}