package org.sf.lang

object Store {
  /**
   * the cell type
   */
  type Cell = (String, Any)
  
  /**
   * An object represents an empty store
   */
  object empty extends Store(("", null)) {
    override val rest = this
    override def toString = "<empty>"
  }

  /**
   * An object represents undefined value
   */
  object undefined {
    override def toString = "<undefined>"
  }
  
  def apply(id: String, v: Any, rest: Store): Store = new Store((id, v), rest)
  
  def apply(head: Cell, rest: Store): Store = new Store(head, rest)
}

/**
 * A class that keeps the configurations. 
 */
class Store(val head: Store.Cell, val rest: Store = Store.empty) {
  import Store._
  
  /**********************************************************
   * the semantics methods
   **********************************************************/

  /**
   * Put a pair (identifier,value) to this store. If the identifier exists, then its
   * value  will be replaced.
   * @param id identifier
   * @param v value
   */
  def put(id: String, v: Any): Store =
    if (this == empty) Store(id, v, empty)
    else if (head._1.equals(id)) Store(id, v, rest)
    else Store(head, rest.put(id, v))
    
  /**
   * Bind a value at given reference to this store.
   * 
   * Errors:
   * - [err1] : the prefix of the reference is not a store
   * - [err2] : an invalid reference
   * - [err3] : the reference cannot be an empty one
   * 
   * @param r a reference where the value will be put
   * @param v value
   */
  def bind(r: Reference, v: Any): Store =
    if (r == Reference.empty) throw new SemanticsException("[err3] invalid reference " + r + "=" + v, 3)
    else if (r.rest == Reference.empty) put(r.head, v)
    else if (this == empty) throw new SemanticsException("[err2] invalid reference " + r + "=" + v, 2)
    else if (head._1.equals(r.head))
      if (head._2.isInstanceOf[Store]) Store(head._1, head._2.asInstanceOf[Store].bind(r.rest, v), rest)
      else throw new SemanticsException("[err1]", 1)
    else Store(head, rest.bind(r, v))
  
  /**
   * Find a value of given reference in this store.
   * 
   * TODO -- handles HERE, ATTRIB
   * 
   * @param r reference of the value
   * @return a value of given reference
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
    
  /**
   * Find a value of given reference in this store within given context (ns).
   * 
   * Error:
   * - [err101] : the reference is invalid
   * 
   * @param ns a reference of the context
   * @param r target reference
   * @return a value
   */
  def resolve(ns: Reference, r: Reference): (Reference, Any) =
  	if (r.head.equals("ROOT")) (Reference.empty, find(r.rest.simplify))
    else if (r.head.equals("PARENT"))
      if (ns == Reference.empty) throw new SemanticsException("[err101] invalid reference: " + r, 101)
      else (ns.prefix, find((ns.prefix ++ r.rest).simplify))
    else if (r.head.equals("THIS")) (ns, find((ns ++ r.rest).simplify))
    else if (ns == Reference.empty) (ns, find(r.simplify))
    else {
      val v = find((ns ++ r).simplify)
      if (v == undefined) resolve(ns.prefix, r)
      else (ns, v)
    }
     
  /**
   * Copy given store (src) to the target reference (r) of this store.
   * @param src a store to be copied
   * @param r target location of copied store
   * @return a store contains the copied store
   */
  def copy(src: Store, r: Reference): Store =
    if (src == empty) this
    else bind(r ++ src.head._1, src.head._2).copy(src.rest, r)

  /**
   * Inherit attributes of a prototype reference 'src' to a target object referred by
   * reference 'dest'. The prototype is resolved within a context reference 'ns'.
   * 
   * @param ns a context reference to resolve the prototype reference
   * @param src a prototype reference
   * @param dest a reference of target object
   * @return a store
   */
  def inherit(ns: Reference, src: Reference, dest: Reference): Store = {
    val l = resolve(ns, src)
    if (l._2.isInstanceOf[Store]) copy(l._2.asInstanceOf[Store], dest)
    else throw new SemanticsException("[err4] invalid prototype reference: " + src, 4)
  }
  
  /**********************************************************
   * helper methods which are not part of semantics
   **********************************************************/

  /**
   * @return a string representation of this store
   */
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
  
  /**
   * @return a JSON representation of this store
   */
  def toJson: String = _toJson(new StringBuffer("{")).append("}").toString

  /**
   * Helper method to generate a JSON representation of this store
   * @param buffer a string buffer of the output
   * @tab a string contains tab-space that needs to be appended to the buffer for pretty printing
   * @return an output buffer
   */
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
  
  /**
   * @return a YAML representation of this store
   */
  def toYaml: String = _toYaml(new StringBuffer("---\n"), "").toString
  
  /**
   * Helper method to generate a YAML representation of this store
   * @param buffer a string buffer of the output
   * @tab a string contains required tab-space that needs to be appended to the buffer
   * @return an output buffer
   */
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

  /**
   * @return a plain SF representation of this store
   */
  def toSf: String = _toSf(new StringBuffer(""), "").toString
  
  /**
   * Helper methods to generate a plain SF.
   * @param buffer a string buffer of the output
   * @tab a string contains tab-space that needs to be appended to the buffer for pretty printing
   * @return an output buffer
   */
  protected def _toSf(buffer: StringBuffer, tab: String): StringBuffer = {
    def value(v: Any, buffer: StringBuffer): StringBuffer =
      if (v.isInstanceOf[List[Any]]) vector(v.asInstanceOf[List[Any]], buffer.append("[|")).append("|]")
      else if (v.isInstanceOf[Reference]) buffer.append(v.asInstanceOf[Reference].toSf)
      else buffer.append(v)
    
    def vector(v: List[Any], buffer: StringBuffer): StringBuffer =
      if (v.length == 0) buffer
      else if (v.tail.isEmpty) value(v.head, buffer)
      else vector(v.tail, value(v.head, buffer).append(", "))
    
    def ident = buffer.append(tab).append(head._1).append(" ")
    
    def _this =
      if (this == empty) buffer
      else if (head._2.isInstanceOf[Store])
        if (head._2.asInstanceOf[Store] == empty)
            ident.append("extends  {}")
        else
          head._2.asInstanceOf[Store]
            ._toSf(ident.append("extends  {\n"), tab + "  ")
            .append('\n')
            .append(tab)
            .append('}')
      else value(head._2, ident).append(';')
    
    if (rest != empty)
      rest._toSf(_this.append("\n"), tab)
    else
      _this
  }
}