package org.sfp.lang

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
    /*try {
      if (inJson(args)) println(parseFile(args.tail.head).toJson)
      else if (inYaml(args)) println(parseFile(args.tail.head).toYaml)
      else println(parseFile(args.head))
    } catch {
      case se: SemanticsException => System.err.println(se.msg)
      case e: Exception => System.err.println(e)
    }*/
  }
    
  def inJson(args: Array[String]): Boolean =
    (args.length >= 2 && args.head.equals("-json"))
    
  def inYaml(args: Array[String]): Boolean =
    (args.length >= 2 && args.head.equals("-yaml"))
}

class Parser extends JavaTokenParsers {
  protected override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

}