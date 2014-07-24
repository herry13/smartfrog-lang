package org.sfp.lang

import org.sf.lang.Reference

object Expression {
  val and = "and"
  val eq = "equals"
  val neq = "not-equals"
  val imply = "imply"
  val in = "in"
}

class PairExpression(override val label: String,
		   			 val left: Reference,
		   			 val right: Any)
	extends Expression(label, List(left, right)) {
}

class Expression(val label: String,
				 val params: List[Any] = List()) {
  override def toString = 
    params.foldLeft[String]("(e:" + label)(
      (s: String, item: Any) => s + " " + item.toString
    ) + ")"

  // find the first found expression with specified label
  def find(label: String): Expression = {
    def innerFind(params: List[Any]): Expression = {
      if (params.isEmpty) null
      else if (params.head.isInstanceOf[Expression] && params.head.asInstanceOf[Expression].label.equals(label))
        params.head.asInstanceOf[Expression]
      else innerFind(params.tail)
    }
    innerFind(params)
  }
  
  // add a parameter at the beginning
  def addParam(p: Any) = new Expression(label, p :: params)
  
  // add a list of parameters at the beginning
  def addParams(ps: List[Any]) = new Expression(label, ps ++ params)
  
  // add a parameter at the beginning of expression referred by given path
  def addParam(path: List[String], p: Any): Expression = {
    def addChildParam(path: List[String], p: Any, children: List[Any], acc: List[Any]): List[Any] = {
      if (children.isEmpty) throw new Exception("invalid path: " + path)
      else if (children.head.isInstanceOf[Expression] && children.head.asInstanceOf[Expression].label.equals(path.head))
        children.tail :: children.head.asInstanceOf[Expression].addParam(p) :: acc
      else addChildParam(path, p, children.tail, children.head :: acc)
    }
    
    if (path.head.equals(label)) {
      if (path.tail.isEmpty) new Expression(label, p :: params)
      else new Expression(label, addChildParam(path.tail, p, params, List()))
    }
    else throw new Exception("invalid path: " + path)
  }
}
