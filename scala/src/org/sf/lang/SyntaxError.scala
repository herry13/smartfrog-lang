package org.sf.lang

class SyntaxError(val filepath: String, val pos: scala.util.parsing.input.Position)
  extends Exception("Syntax error in file " + filepath +  " at " + pos) {
  
}