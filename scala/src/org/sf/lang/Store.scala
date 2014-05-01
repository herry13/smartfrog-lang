package org.sf.lang

object Store {  
  type Cell = (String, Any)
  
  object empty extends Store(("", null)) {
    override val rest = this
    override def toString = "<empty>"
  }

  object undefined {
    override def toString = "<undefined>"
  }
  
  def apply(id: String, v: Any, rest: Store): Store = new Store((id, v), rest)
  
  def apply(head: Cell, rest: Store): Store = new Store(head, rest)
}

class Store(val head: Store.Cell, val rest: Store = Store.empty) {
  import Store._
  
  //--- merge ---//
  def put(id: String, v: Any): Store =
    if (this == empty) Store(id, v, empty)
    else if (head._1.equals(id))
      if (v.isInstanceOf[Store] && head._2.isInstanceOf[Store])
        Store(id, head._2.asInstanceOf[Store].copy(v.asInstanceOf[Store], Reference.empty), rest)
      else Store(id, v, rest)
    else Store(head, rest.put(id, v))
  
  //--- start of semantics functions ---//
  /*
  def put(id: String, v: Any): Store =
    if (this == empty) Store(id, v, empty)
    else if (head._1.equals(id)) Store(id, v, rest)
    else Store(head, rest.put(id, v))*/
    
  def bind(r: Reference, v: Any): Store =
    if (r == Reference.empty) throw new SemanticsException("[err3] invalid reference " + r + "=" + v)
    else if (r.rest == Reference.empty) put(r.head, v)
    else if (this == empty) throw new SemanticsException("[err2] invalid reference " + r + "=" + v)
    else if (head._1.equals(r.head))
      if (head._2.isInstanceOf[Store]) Store(head._1, head._2.asInstanceOf[Store].bind(r.rest, v), rest)
      else throw new SemanticsException("[err1]")
    else Store(head, rest.bind(r, v))
  
  /**
   * Find a value of given reference.
   * TODO -- handles HERE, ATTRIB
   * 
   * @param r reference of the value
   */
  def find(r: Reference): Any =
    if (this == empty) undefined
    else if (r == Reference.empty) this
    else if (r.rest == Reference.empty)
      if (head._1.equals(r.head)) head._2
      else rest.find(r)
    else if (head._1.equals(r.head))
      if (head._2.isInstanceOf[Store]) head._2.asInstanceOf[Store].find(r.rest)
      else undefined
    else rest.find(r)
    
  def resolve(ns: Reference, r: Reference): (Reference, Any) =
  	if (r.head.equals("ROOT")) (Reference.empty, find(r.rest.simplify))
    else if (r.head.equals("PARENT"))
      if (ns == Reference.empty) throw new SemanticsException("[err101] invalid reference: " + r)
      else (ns.prefix, find((ns.prefix ++ r.rest).simplify))
    else if (r.head.equals("THIS")) (ns, find((ns ++ r.rest).simplify))
    else if (ns == Reference.empty) (ns, find(r.simplify))
    else {
      val v = find((ns ++ r).simplify)
      if (v == undefined) resolve(ns.prefix, r)
      else (ns, v)
    }
      
  def copy(src: Store, r: Reference): Store =
    if (src == empty) this
    else bind(r ++ src.head._1, src.head._2).copy(src.rest, r)

  def inherit(ns: Reference, src: Reference, dest: Reference): Store = {
    val l = resolve(ns, src)
    if (l._2.isInstanceOf[Store]) copy(l._2.asInstanceOf[Store], dest)
    else throw new SemanticsException("[err4] invalid prototype reference: " + src)
  }
  //--- end of semantics functions ---//
  
  //--- helper functions (not part of semantics) ---//
  override def toString = {
    def vectorToString(v: List[Any]): String =
      if (v.length == 0) ""
      else if (v.tail.isEmpty) valueToString(v.head)
      else valueToString(v.head) + "," + vectorToString(v.tail)
    
    def valueToString(v: Any): String =
      if (v.isInstanceOf[Store]) "{" + v + "}"
      else if (v.isInstanceOf[List[Any]]) "[" + vectorToString(v.asInstanceOf[List[Any]]) + "]"
      else v.toString
    
    "(" + head._1 + "," + valueToString(head._2) + ")" + (if (rest == empty) "" else "," + rest)
  }
    
  def toJson: String = _toJson(new StringBuffer("{")).append("}").toString

  protected def _toJson(buffer: StringBuffer): StringBuffer = {
    def value(v: Any, buffer: StringBuffer): StringBuffer =
      if (v.isInstanceOf[List[Any]]) vector(v.asInstanceOf[List[Any]], buffer.append("[")).append("]")
      else if (v.isInstanceOf[Reference]) buffer.append(v.asInstanceOf[Reference].toJson)
      else buffer.append(v)
    
    def vector(v: List[Any], buffer: StringBuffer): StringBuffer =
      if (v.length == 0) buffer
      else if (v.tail.isEmpty) value(v.head, buffer)
      else vector(v.tail, value(v.head, buffer).append(","))
    
    def ident = buffer.append("\"").append(head._1).append("\":")
    
    def _this =
      if (this == empty)
        buffer
      else if (head._2.isInstanceOf[Store])
        head._2.asInstanceOf[Store]._toJson(ident.append("{")).append("}")
      else
        value(head._2, ident)
    
    if (rest != empty)
      rest._toJson(_this.append(","))
    else
      _this
  }
  
  def toYaml: String = _toYaml(new StringBuffer("---\n"), "").toString
  
  protected def _toYaml(buffer: StringBuffer, tab: String): StringBuffer = {
    def value(v: Any, buffer: StringBuffer): StringBuffer =
      if (v.isInstanceOf[List[Any]]) vector(v.asInstanceOf[List[Any]], buffer.append("[")).append("]")
      else if (v.isInstanceOf[Reference]) buffer.append(v.asInstanceOf[Reference].toYaml)
      else buffer.append(v)
    
    def vector(v: List[Any], buffer: StringBuffer): StringBuffer =
      if (v.length == 0) buffer
      else if (v.tail.isEmpty) value(v.head, buffer)
      else vector(v.tail, value(v.head, buffer).append(","))
    
    def ident = buffer.append(tab).append(head._1).append(": ")
    
    def _this =
      if (this == empty) buffer
      else if (head._2.isInstanceOf[Store])
        head._2.asInstanceOf[Store]._toYaml(ident.append("\n"), tab + "  ")
      else value(head._2, ident)
    
    if (rest != empty)
      rest._toYaml(_this.append("\n"), tab)
    else
      _this
  }
}