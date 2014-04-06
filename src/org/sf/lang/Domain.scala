package org.sf.lang

object Domain {
  object TBD {
    override def toString = "[TBD]"
  }
  
  type Vector = List[Any]

  /** Reference and LinkReference domains **/
  type Reference = List[String]
  
  type LinkReference = (Reference, Any)
  
  def prefix(r: Reference): Reference = 
    if (r.isEmpty || r.tail.isEmpty) List()
    else r.head :: prefix(r.tail)
    
  def ominus(r1: Reference, r2: Reference): Reference =
    if (r1.isEmpty || r2.isEmpty) r1
    else if (r1.head.equals(r2.head)) ominus(r1.tail, r2.tail)
    else r1
    
  def subseteq(r1: Reference, r2: Reference): Boolean =
    ominus(r1, r2).isEmpty

  /** Store domain **/
  type Store = (String => Any)
  
  object Undefined {
    override def toString = "[undefined]"
  }
  
  val Empty: Store = new Function1[String, Any] {
    def apply(id: String): Any = Undefined
    override def toString = "[empty]"
    def toJson = toString
    def toYaml = toString
  }

  def replaceLink(s: Store): Store = ??? // TODO
  
  def put(s: Store, id: String, v: Any): Store = new Function1[String, Any] {
    def apply(x: String): Any = if (x.equals(id)) v else s(x)
    override def toString =
      "(" + id + "," + v + ")" + "," + s
    def toJson = ???
    def toYaml = ???
  }
  
  def bind(s: Store, r: Reference, v: Any): Store =
    if (r.isEmpty) throw new Exception
    else if (r.tail.isEmpty) put(s, r.head, v)
    else {
      ((v1: Any) =>
        if (!v1.isInstanceOf[Store]) throw new Exception
        else put(s, r.head, bind(v1.asInstanceOf[Store], r.tail, v))
      )(s(r.head))
    } 
    
  def find(s: Store, r: Reference): Any =
    if (r.isEmpty) s
    else {
      ((v: Any) =>
        if (r.tail.isEmpty) v
        else if (v.isInstanceOf[Store]) find(v.asInstanceOf[Store], r.tail)
        else Undefined
      )(s(r.head))
    }
  
  def resolve(s: Store, ns: Reference, r: Reference): (Reference, Any) =
    if (ns.isEmpty) (ns, find(s, r))
    else {
      ((v: Any) =>
        if (v == Undefined) resolve(s, prefix(ns), r)
        else (ns, v)
      )(find(s, ns ++ r))
    }

  def inherit(s: Store, ns: Reference, r1: Reference, r2: Reference): Store =
    bind(s, r2, inherit1(s, ns, r1, r2))
    
  def inherit1(s: Store, ns: Reference, r1: Reference, r2: Reference): Store =
    ((src: (Reference, Any), dest: Any) =>
      if (!src._2.isInstanceOf[Store]) throw new Exception("src is not store: " + src._2)
      else if (!dest.isInstanceOf[Store]) throw new Exception("dest is not store: " + dest)
      else new Function1[String, Any] {
        override def apply(x: String): Any =
          if (src._2.asInstanceOf[Store](x) != Undefined) src._2.asInstanceOf[Store](x)
          else dest.asInstanceOf[Store](x)
        override def toString = src + "," + dest
        def toJson = ???
        def toYaml = ???
      }
    )(resolve(s, ns, r1), find(s, r2))
}