package org.sf.lang

object Store {  
  type Cell = (String, Any)
  
  object Empty extends Store(("", null)) {
    override val rest = this
    override def toString = "[Empty]"
  }

  object Undefined extends Store(("", null)) {
    override val rest = this
    override def toString = "[Undefined]"
  }
  
  def apply(id: String, v: Any, rest: Store): Store = new Store((id, v), rest)
  
  def apply(head: Cell, rest: Store): Store = new Store(head, rest)

  def isStore(v: Any): Boolean = (v.isInstanceOf[Store] && v != Undefined)
}

class Store(val head: Store.Cell, val rest: Store = Store.Empty) {
  import Store._
  
  //--- start of semantics functions ---//
  def put(id: String, v: Any): Store =
    if (this == Empty) Store(id, v, Empty)
    else if (head._1.equals(id)) Store(id, v, rest)
    else Store(head, rest.put(id, v))
    
  def bind(r: Reference, v: Any): Store =
    if (r == Reference.Empty) throw new SemanticsException("[err3] invalid reference " + r + "=" + v)
    else if (r.rest == Reference.Empty) put(r.head, v)
    else if (this == Empty) throw new SemanticsException("[err2] invalid reference " + r + "=" + v)
    else
      if (head._1.equals(r.head))
        if (isStore(head._2)) Store(head._1, head._2.asInstanceOf[Store].bind(r.rest, v), rest)
        else throw new SemanticsException("[err1]")
      else Store(head, rest.bind(r, v))
  
  /**
   * Find a value of given reference.
   * 
   * @param r reference of the value
   * 
   * TODO -- handles ROOT, PARENT, HERE, THIS, ATTRIB
   */
  def find(r: Reference): Any =
    if (this == Empty) Undefined
    else if (r.rest == Reference.Empty)
      if (head._1.equals(r.head)) head._2
      else rest.find(r)
    else
      if (head._1.equals(r.head))
        if (isStore(head._2)) head._2.asInstanceOf[Store].find(r.rest)
        else Undefined
      else rest.find(r)
  
  def resolve(ns: Reference, r: Reference): (Reference, Any) =
    if (ns == Reference.Empty) (ns, find(r))
    else
      ((v: Any) =>
        if (v == Undefined) resolve(ns.prefix, r)
        else (ns, v)
      )( find(ns ++ r) )
      
  def copy(src: Store, r: Reference): Store =
    if (src == Empty) this
    else bind(r ++ src.head._1, src.head._2).copy(src.rest, r)

  def inherit(ns: Reference, src: Reference, dest: Reference): Store = {
    ((l: (Reference, Any)) =>
      if (isStore(l._2)) copy(l._2.asInstanceOf[Store], dest)
      else throw new SemanticsException("[err4] invalid prototype reference: " + src)
    )(resolve(ns, src))
  }
  //--- end of semantics functions ---//
  
  //--- helper functions (not part of semantics) ---//
  override def toString = {
    def vectorToString(v: List[Any]): String =
      if (v.length == 0) ""
      else if (v.tail.isEmpty) valueToString(v.head)
      else valueToString(v.head) + "," + vectorToString(v.tail)
    
    def valueToString(v: Any): String =
      if (isStore(v)) "{" + v + "}"
      else if (v.isInstanceOf[List[Any]]) "[" + vectorToString(v.asInstanceOf[List[Any]]) + "]"
      else v.toString
    
    "(" + head._1 + "," + valueToString(head._2) + ")" + (if (rest == Empty) "" else "," + rest)
  }
    
  def toJson: String = _toJson(new StringBuffer("{")).append("}").toString

  protected def _toJson(buffer: StringBuffer): StringBuffer = {
    def value(v: Any, buffer: StringBuffer): StringBuffer =
      if (v.isInstanceOf[List[Any]]) vector(v.asInstanceOf[List[Any]], buffer.append("[")).append("]")
      else if (Reference.isReference(v)) buffer.append(v.asInstanceOf[Reference].toJson)
      else buffer.append(v)
    
    def vector(v: List[Any], buffer: StringBuffer): StringBuffer =
      if (v.length == 0) buffer
      else if (v.tail.isEmpty) value(v.head, buffer)
      else vector(v.tail, value(v.head, buffer).append(","))
    
    def ident = buffer.append("\"").append(head._1).append("\":")
    
    def _this =
      if (this == Empty)
        buffer
      else if (isStore(head._2))
        head._2.asInstanceOf[Store]._toJson(ident.append("{")).append("}")
      else
        value(head._2, ident)
    
    if (rest != Empty)
      rest._toJson(_this.append(","))
    else
      _this
  }
  
  def toYaml: String = _toYaml(new StringBuffer("---\n"), "").toString
  
  protected def _toYaml(buffer: StringBuffer, tab: String): StringBuffer = {
    def value(v: Any, buffer: StringBuffer): StringBuffer =
      if (v.isInstanceOf[List[Any]]) vector(v.asInstanceOf[List[Any]], buffer.append("[")).append("]")
      else if (Reference.isReference(v)) buffer.append(v.asInstanceOf[Reference].toYaml)
      else buffer.append(v)
    
    def vector(v: List[Any], buffer: StringBuffer): StringBuffer =
      if (v.length == 0) buffer
      else if (v.tail.isEmpty) value(v.head, buffer)
      else vector(v.tail, value(v.head, buffer).append(","))
    
    def ident = buffer.append(tab).append(head._1).append(": ")
    
    def _this =
      if (this == Empty) buffer
      else if (isStore(head._2))
        head._2.asInstanceOf[Store]._toYaml(ident.append("\n"), tab + "  ")
      else value(head._2, ident)
    
    if (rest != Empty)
      rest._toYaml(_this.append("\n"), tab)
    else
      _this
  }
}