package org.sf.lang

import scala.util.parsing.combinator.JavaTokenParsers

class Parser extends JavaTokenParsers {
  protected override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r
  
  def Stream: Parser[Any] = Body ^^ (x => ???)

  def Body: Parser[Any] = AttributeList ^^ (x => ???)
  
  def AttributeList: Parser[List[Any]] =
    ( Attribute ~ AttributeList ^^ { case attr ~ attrList => attr :: attrList }
    //| "#include" ~> stringLiteral ~ AttributeList ^^ (x => ???)
    //| "#include?" ~> stringLiteral ~ AttributeList ^^ (x => ???)
    | ";" ~> AttributeList
    | (epsilon | "\\Z".r) ^^ (x => List())
    )

  def Attribute: Parser[Any] =
    Name ~ Value ^^ { case name ~ value => ??? }
	
  def Name: Parser[List[Any]] =
    ( "--" ^^ (x => List("x" + scala.util.Random.alphanumeric.take(5).mkString))
    | Word ~ (":" ~> Name).? ^^ { case word ~ name =>
      	if (name.isEmpty) List(word)
      	else word :: name.last
      }
    )

  def Word: Parser[String] = ident

  def Value: Parser[Any => Any] =
    ( Component ^^ (v => (r: Any) => ???)
    | SimpleValue <~ ";"  ^^ (v => (r: Any) => ???)
    | LinkReference <~ ";"  ^^ (v => (r: Any) => ???)
    | ";" ^^ { x => (r: Any) => ??? } 
    )
	
  def SimpleValue: Parser[Any] =
    ( Basic
    | TBD
    //| Function  // TODO
    //| Predicate  // TODO
    )

  def Basic: Parser[Any] =
    ( stringLiteral ^^ (x => ???)
    | Number ^^ (x => ???)
    | Boolean ^^ (x => ???)
    | ByteArray ^^ (x => ???)
    | ReferenceValue ^^ (x => ???)
    | BasicVector ^^ (x => ???)
    | Null ^^ (x => ???)
    )

  val TBD: Parser[Any] = "TBD" ^^ (x => ???)
    
  protected val intRegex = """\-?[0-9]+(?!\.)""".r
  
  def Number: Parser[Any] =
    floatingPointNumber ^^ (n =>
      if (intRegex.findFirstIn(n).get != n) n.toDouble
      else n.toInt
    )

  def BasicVector: Parser[Any] =
    "[|" ~>
    ( Basic ~ ("," ~> Basic).* ^^ { case x ~ xs => ??? }
    | epsilon ^^ (x => ???)
    ) <~ "|]"

  def Null: Parser[Any] = "NULL" ^^ (x => ???)
    
  def Vector: Parser[Any] =
    "[" ~>
    ( SimpleValue ~ ("," ~> SimpleValue).* ^^ { case x ~ xs => ??? }
    | epsilon ^^ (x => ???)
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

  def ReferenceValue: Parser[Any] =
    "DATA" ~> BaseReference ^^ (r => ???)

  def BaseReference: Parser[Any] =
    ReferencePart ~ (":" ~> ReferencePart).* ^^ { case r ~ rs => ??? }
	
  def ReferencePart: Parser[String] =
    ( "ROOT" ^^ (x => "ROOT")
    | "PARENT" ^^ (x => "PARENT")
    | "ATTRIB" ~> Word ^^ (x => "ATTRIB")
    | "HERE" ~> Word ^^ (x => "HERE")
    | "THIS" ^^ (x => "THIS")
    | Word ^^ (x => x)
    )
	
  def LinkReference: Parser[Any] =
    OptionalValueLinkReference.? ~ BaseReference ^^ { case opt ~ r =>
      if (opt.isEmpty) ???
      else ???
    }

  def OptionalValueLinkReference: Parser[Any] =
    "OPTIONAL" ~> SimpleValue ^^ (x => ???)
        
  def Component: Parser[Any] =
    "extends" ~> Prototypes ^^ (p => ???)

  def Prototypes: Parser[Any] =
    ( Prototype ~ ("," ~> Prototype).* ^^ { case p ~ ps => ??? }
    | epsilon ^^ (x => ???)
    )
    
  def Prototype: Parser[Any] =
    ( BaseReference
    | "{" ~> AttributeList <~ "}" ^^ (attrs => ???)
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
    parseAll(Stream, s) match {
      case Success(root, _) => root
      case NoSuccess(msg, next) => throw new Exception("at " + next.pos)
    }
  }
    
  def parseFile(filePath: String) = parse(scala.io.Source.fromFile(filePath).mkString)
}