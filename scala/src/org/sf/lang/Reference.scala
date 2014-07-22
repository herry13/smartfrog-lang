package org.sf.lang

object Reference {
  /**
   * An object represents an empty reference.
   */
  object empty extends Reference("") {
	override val rest = this
	override def last = throw new Exception
	override def prefix = throw new Exception
	override def equals(that: Reference) = (this == that)
	override def toString = "[empty]"
  }
  
  def apply(s: String): Reference = new Reference(s, empty)
  
  def apply(s: String, rest: Reference): Reference = new Reference(s, rest)
  
  def apply(rs: List[String]): Reference =
    rs.foldLeft[Reference](Reference.empty)((r: Reference, s: String) => r ++ s)
}

/**
 * A class that represents a reference.
 */
class Reference(val head: String, val _rest: Reference = Reference.empty) {
  import Reference._
  
  val rest = _rest

  /**********************************************************
   * the semantics methods
   **********************************************************/
  
  /**
   * Append 'that' to 'this' at the last position.
   * @param that a reference to be appended to 'this'
   * @return a reference 'this' plus 'that'
   */
  def ++(that: Reference): Reference =
    if (this == empty) that
    else if (that == empty) this
    else Reference(head, this.rest ++ that)

  /**
   * Append identifier 'that' to the last position.
   * @param that an identifier to be appended to 'this'
   * @return a reference 'this' plus 'that'
   */
  def ++(that: String): Reference =
    if (this == empty) Reference(that, empty)
    else Reference(this.head, this.rest ++ that)
  
  /**
   * @return the last identifier
   */
  def last: String =
    if (rest == empty) head
    else rest.last

  /**
   * @return the length (the number of identifiers) of 'this'
   */
  def length: Integer =
    if (this == empty) 0
    else 1 + rest.length
    
  /**
   * @return the prefix of 'this' reference by removing the last identifier. 
   */
  def prefix: Reference =
    if (rest == empty) empty
    else if (rest.rest == empty) Reference(head)
    else Reference(head, rest.prefix)
    
  /**
   * Remove the longest common prefix (with 'that') from 'this' reference.
   * @param that a reference to be checked the common prefix with
   * @return a reference which does not have common prefix with 'that'
   */
  def --(that: Reference): Reference =
    if (this == empty || that == empty) this
    else if (this.head == that.head) this.rest -- that.rest
    else this
    
  /**
   * Remove the first identifier of 'this' if it is equal to identifier 'that'
   * @param that an identifier to be checked
   * @return a reference whose first identifier is not equal to 'that'
   */
  def --(that: String): Reference =
    if (this == empty || that == empty) this
    else if (this.head.equals(that)) this.rest
    else this
    
  /**
   * An alias operator of 'subseteqof'
   * @param that a reference to be compared
   * @return true if given reference is a subset of equals to this reference, otherwise false 
   */
  def <=(that: Reference) = subseteqof(that)

  /**
   * @param that a reference to be compared
   * @return true if given reference is a subset of equals to this reference, otherwise false 
   */
  def subseteqof(that: Reference): Boolean = (this -- that) == empty
  
  /**
   * @param that a reference to be compared
   * @return true if this equals to given reference, otherwise false
   */
  def equals(that: Reference): Boolean =
    if (that == empty) false
    else if (head.equals(that.head)) rest.equals(that.rest)
    else false

  /**
   * @param id an identifier to be searched in this reference
   * @return true if given identifier 'id' exists in the path, otherwise false
   */
  def exist(id: String): Boolean =
    if (head.equals(id)) true
    else if (rest == empty) false
    else rest.exist(id)
    
  /**
   * Make reference 'r' to have an absolute path within 'this' context (as namespace)
   * by removing THIS, ROOT, and PARENT identifiers. It returns a reference without these
   * identifiers.
   * @param r a reference to be having an absolute path
   * @return a reference with an absolute path within 'this' context
   */
  def trace(r: Reference): Reference =
    if (r == empty) this
    else if (r.head.equals("THIS")) trace(r.rest)
    else if (r.head.equals("ROOT")) empty.trace(r.rest)
    else if (r.head.equals("PARENT"))
      if (this == empty) throw new SemanticsException("[err102] cannot trace " + r + " in " + this, 102)
      else prefix.trace(r.rest)
    else (this ++ r.head).trace(r.rest)

  /**
   * Use 'trace' to make 'this' to have an absolute path within 'this' context.
   * @return a reference with an absolute path within 'this' context
   */
  def simplify: Reference = empty.trace(this)
    
  /**********************************************************
   * helper methods which are not part of semantics
   **********************************************************/
  
  /**
   * @return a string representation of this reference
   */
  override def toString = if (rest == empty) head else head + ":" + rest
  
  /**
   * @return a JSON representation of this reference
   */
  def toJson: String = "\"$." + toString + "\""

  /**
   * @return a YAML representation of this reference
   */
  def toYaml: String = toJson
  
  /**
   * @return a plain SF representation of this reference
   */
  def toSf: String = "DATA " + toString
}