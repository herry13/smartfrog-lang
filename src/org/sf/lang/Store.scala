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
  
  object TBD extends Store(("", null)) {
    override val rest = this
    override def toString = "[TBD]"
  }
  
  def apply(id: String, v: Any, rest: Store): Store = new Store((id, v), rest)
  
  def apply(head: Cell, rest: Store): Store = new Store(head, rest)

  def replaceLink(s: Store, ns: Reference, c: Cell): Any =
    if (c._2.isInstanceOf[LinkReference])
      ((v: Any) =>
        if (v == Undefined) throw new Exception("invalid link reference: " + c._2)
        else v
      )(s.resolvelink(ns, ns ++ c._1, c._2.asInstanceOf[LinkReference]))
    else c._2
    
  def isStore(v: Any): Boolean = (v.isInstanceOf[Store] && v != Undefined)
}

class Store(val head: Store.Cell, val rest: Store = Store.Empty) {
  import Store._
  
  def put(id: String, v: Any): Store =
    if (this == Empty) Store(id, v, Empty)
    else if (head._1.equals(id)) Store(id, v, rest)
    else Store(head, rest.put(id, v))
    
  def bind(r: Reference, v: Any): Store =
    if (r == Reference.Empty) throw new Exception("invalid reference: " + r + "=" + v)
    else if (r.rest == Reference.Empty) put(r.head, v)
    else if (this == Empty) throw new Exception("invalid reference: " + r + "=" + v)
    else
      if (head._1.equals(r.head))
        if (isStore(head._2)) Store(head._1, head._2.asInstanceOf[Store].bind(r.rest, v), rest)
        else throw new Exception
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
      else if (l._2.isInstanceOf[LinkReference])
        ((v: Any) =>
          if (isStore(v)) copy(v.asInstanceOf[Store], dest)
          else throw new Exception("invalid prototype reference: " + src)
        )(resolvelink(ns, dest, new LinkReference(src)))
      else throw new Exception("invalid prototype reference: " + src)
    )(resolve(ns, src))
  }
  
  def resolvelink(ns: Reference, r: Reference, lr: LinkReference): Any = {
    def getlink(ns: Reference, lr: LinkReference, acc: Set[LinkReference]): Any = {
      if (acc.contains(lr)) throw new Exception("cyclic link reference is detected: " + lr + ", visited: " + acc)
      else {
        ((l: (Reference, Any)) =>
          if (l._2.isInstanceOf[LinkReference])
            getlink((l._1 ++ lr.ref).prefix, l._2.asInstanceOf[LinkReference], acc + lr)
          else if ((l._1 ++ lr.ref).subseteqof(r)) throw new Exception("implicit cyclic link reference")
          else l._2
        )(resolve(ns, lr.ref))
      }
    }
    getlink(ns, lr, Set())
  }
  
  def accept(visitor: (Store, Reference, Cell) => Any): Store =
    accept1(this, Reference.Empty, visitor)
    
  def accept1(root: Store, ns: Reference, visitor: (Store, Reference, Cell) => Any): Store = {
    if (this == Empty) root
    else if (isStore(head._2))
      rest.accept1(head._2.asInstanceOf[Store].accept1(root, ns ++ head._1, visitor) , ns, visitor)
    else {
      ((v: Any) =>
        ((root1: Store) =>
          if (isStore(v))
            rest.accept1(
                v.asInstanceOf[Store].accept1(root1, ns ++ head._1, visitor)
                ,ns, visitor
              )
          else rest.accept1(root1, ns, visitor)
        )(root.bind(ns ++ head._1, v))
      )(visitor(root, ns, head))
    }
  }
  
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