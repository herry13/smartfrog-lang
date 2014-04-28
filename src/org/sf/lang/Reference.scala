package org.sf.lang

object Reference {
  object empty extends Reference("") {
	override val rest = this
	override def last = throw new Exception
	override def prefix = throw new Exception
	override def equals(that: Reference) = (this == that)
	override def toString = "[empty]"
  }
  
  def apply(s: String): Reference = new Reference(s, empty)
  
  def apply(s: String, rest: Reference): Reference = new Reference(s, rest)
  
  def apply(rs: List[String]): Reference = rs.foldLeft[Reference](Reference.empty)((r: Reference, s: String) => r ++ s)
}

class Reference(val head: String, val _rest: Reference = Reference.empty) {
  import Reference._
  
  val rest = _rest

  def ++(that: Reference): Reference =
    if (this == empty) that
    else if (that == empty) this
    else Reference(head, this.rest ++ that)

  def ++(that: String): Reference =
    if (this == empty) Reference(that, empty)
    else Reference(this.head, this.rest ++ that)
  
  def last: String =
    if (rest == empty) head
    else rest.last

  def length: Integer =
    if (this == empty) 0
    else 1 + rest.length
    
  def prefix: Reference =
    if (rest == empty) empty
    else if (rest.rest == empty) Reference(head)
    else Reference(head, rest.prefix)

  def --(that: Reference): Reference =
    if (this == empty || that == empty) this
    else if (this.head == that.head) this.rest -- that.rest
    else this
    
  def --(that: String): Reference =
    if (this == empty || that == empty) this
    else if (this.head.equals(that)) this.rest
    else this
    
  def <=(that: Reference) = subseteqof(that)
    
  def subseteqof(that: Reference): Boolean = (this -- that) == empty
  
  def equals(that: Reference): Boolean =
    if (that == empty) false
    else if (head.equals(that.head)) rest.equals(that.rest)
    else false

  def exist(id: String): Boolean =
    if (head.equals(id)) true
    else if (rest == empty) false
    else rest.exist(id)
  
  def delete(id: String): Reference =
    if (this == empty) this
    else if (head.equals(id)) rest.delete(id)
    else new Reference(head, rest.delete(id))
    
  override def toString = if (rest == empty) head else head + ":" + rest
  
  def toJson: String = "\"$." + toString + "\""
  
  def toYaml: String = toJson
}