package org.sf.lang

object Reference {
  object Empty extends Reference("[empty]") {
	override val rest = Empty.this
	override def last = throw new Exception
	override def prefix = throw new Exception
	override def equals(that: Reference) = (Empty.this == that)
	override def toString = "[empty]"
  }
  
  object Undefined extends Reference("[undefined]") {
	override val rest = Undefined.this
	override def last = throw new Exception
	override def prefix = throw new Exception
	override def equals(that: Reference) = (Undefined.this == that)
	override def toString = "[undefined]"
  }
  
  def apply(s: String): Reference = new Reference(s, Empty)
  
  def apply(s: String, rest: Reference): Reference = new Reference(s, rest)
  
  def apply(rs: List[String]): Reference = rs.foldLeft[Reference](Reference.Empty)((r: Reference, s: String) => r ++ s)  
}

class Reference(val head: String, val _rest: Reference = Reference.Empty) {
  import Reference._
  
  val rest = _rest

  def ++(that: Reference): Reference =
    if (this == Empty) that
    else if (that == Empty) this
    else Reference(head, this.rest ++ that)

  def ++(that: String): Reference =
    if (this == Empty) Reference(that, Empty)
    else Reference(this.head, this.rest ++ that)
  
  def last: String =
    if (rest == Empty) head
    else rest.last

  def length: Integer =
    if (this == Empty) 0
    else 1 + rest.length
    
  def prefix: Reference =
    if (rest == Empty) Empty
    else if (rest.rest == Empty) Reference(head)
    else Reference(head, rest.prefix)

  def --(that: Reference): Reference =
    if (this == Empty || that == Empty) this
    else if (this.head == that.head) this.rest -- that.rest
    else this
    
  def --(that: String): Reference =
    if (this == Empty || that == Empty) this
    else if (this.head.equals(that)) this.rest
    else this
    
  def subseteqof(that: Reference): Boolean = (this -- that) == Empty
  
  def equals(that: Reference): Boolean =
    if (that == Empty) false
    else if (head.equals(that.head)) rest.equals(that.rest)
    else false

  def exist(id: String): Boolean =
    if (head.equals(id)) true
    else if (rest == Empty) false
    else rest.exist(id)
  
  def delete(id: String): Reference =
    if (this == Empty) this
    else if (head.equals(id)) rest.delete(id)
    else new Reference(head, rest.delete(id))
    
  override def toString = if (rest == Empty) head else head + ":" + rest
}