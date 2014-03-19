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
      )(s.resolvelink(ns, c._2.asInstanceOf[LinkReference]))
    else c._2
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
        if (head._2.isInstanceOf[Store]) Store(head._1, head._2.asInstanceOf[Store].bind(r.rest, v), rest)
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
        if (head._2.isInstanceOf[Store]) head._2.asInstanceOf[Store].find(r.rest)
        else Undefined
      else rest.find(r)
  
  def copy(src: Store, r: Reference): Store =
    if (src == Empty) this
    else bind(r ++ src.head._1, src.head._2).copy(src.rest, r)

  def inherit(ns: Reference, src: Reference, dest: Reference): Store =
    ((l: (Reference, Any)) =>
      if (l._2.isInstanceOf[Store]) copy(l._2.asInstanceOf[Store], dest)
      else if (l._2.isInstanceOf[LinkReference])
        ((v: Any) =>
          if (v.isInstanceOf[Store]) copy(v.asInstanceOf[Store], dest)
          else throw new Exception("invalid prototype reference: " + src)
        )(resolvelink(ns, new LinkReference(src)))
      else throw new Exception("invalid prototype reference: " + src)
    )(resolve(ns, src))
  
  def resolve(ns: Reference, r: Reference): (Reference, Any) =
    if (ns == Reference.Empty) (ns, find(r))
    else
      ((v: Any) =>
        if (v == Undefined) resolve(ns.prefix, r)
        else (ns, v)
      )( find(ns ++ r) )
      
  def resolvelink(ns: Reference, lr: LinkReference): Any = {
    def getlink1(ns: Reference, lr: LinkReference, acc: Set[LinkReference]): Any = {
      if (acc.contains(lr)) throw new Exception("cyclic link reference is detected: " + lr + ", visited: " + acc)
      else {
        ((l: (Reference, Any)) =>
          if (l._2.isInstanceOf[LinkReference])
            getlink1((l._1 ++ lr.ref).prefix, l._2.asInstanceOf[LinkReference], acc + lr)
          else l._2
        )(resolve(ns, lr.ref))
      }
    }
    getlink1(ns, lr, Set())
  }
  
  def accept(visitor: (Store, Reference, Cell) => Any): Store =
    accept1(this, Reference.Empty, visitor)
    
  def accept1(root: Store, ns: Reference, visitor: (Store, Reference, Cell) => Any): Store = {
    if (this == Empty) root
    else if (head._2.isInstanceOf[Store])
      rest.accept1(head._2.asInstanceOf[Store].accept1(root, ns ++ head._1, visitor) , ns, visitor)
    else {
      ((v: Any) =>
        ((root1: Store) =>
          if (v.isInstanceOf[Store])
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
    def valueToString(v: Any): String =
      if (v.isInstanceOf[Store]) "{" + v + "}"
      else v.toString
    
    "(" + head._1 + "," + valueToString(head._2) + ")" + (if (rest == Empty) "" else "," + rest)
  }
    
}