package org.sf.lang

object Domain {
  type FStore = (String => Any)
  object Undefined {
    override def toString = "[undefined]"
  }
  def Empty: FStore = (id: String) => Undefined

  def put(s: FStore, id: String, v: Any): FStore =
    (id2: String) => if (id2.equals(id)) v else s(id2)
  
  def bind(s: FStore, r: Reference, v: Any): FStore =
    if (r == Reference.Empty) throw new Exception
    else if (r.rest == Reference.Empty) put(s, r.head, v)
    else {
      ((v1: Any) =>
        if (!v1.isInstanceOf[FStore]) throw new Exception
        else put(s, r.head, bind(v1.asInstanceOf[FStore], r.rest, v))
      )(s(r.head))
    } 
    
  def find(s: FStore, r: Reference): Any =
    if (r == Reference.Empty) s
    else {
      ((v: Any) =>
        if (r.rest == Reference.Empty) v
        else if (v.isInstanceOf[FStore]) find(v.asInstanceOf[FStore], r.rest)
        else Undefined
      )(s(r.head))
    }
  
  def resolve(s: FStore, ns: Reference, r: Reference): (Reference, Any) =
    if (ns == Reference.Empty) (ns, find(s, r))
    else {
      ((v: Any) =>
        if (v == Undefined) resolve(s, ns.prefix, r)
        else (ns, v)
      )(find(s, ns ++ r))
    }

  def inherit(s: FStore, ns: Reference, r1: Reference, r2: Reference): FStore =
    bind(s, r2, inherit1(s, ns, r1, r2))
    
  def inherit1(s: FStore, ns: Reference, r1: Reference, r2: Reference): FStore =
    ((src: (Reference, Any), dest: Any) =>
      if (!src._2.isInstanceOf[FStore]) throw new Exception("src is not store: " + src._2)
      else if (!dest.isInstanceOf[FStore]) throw new Exception("dest is not store: " + dest)
      else {
        (id: String) =>
          if (src._2.asInstanceOf[FStore](id) != Undefined) src._2.asInstanceOf[FStore](id)
          else dest.asInstanceOf[FStore](id)
      }
    )(resolve(s, ns, r1), find(s, r2))
}