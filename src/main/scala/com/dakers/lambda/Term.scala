package com.dakers.lambda

/** Represents the grammar
 * Λ = V |(ΛΛ)|(λV . Λ)
 *
 * Nederpelt, Rob. Type Theory and Formal Proof: An Introduction (Kindle Locations 645-646). Cambridge University Press. Kindle Edition.
 */
abstract class Term {
  def free(): Set[String]

  def bound(): Set[String]

}

object Term {
  val AbstOp = "/|"
  val AbstSep = "."

  /** Prevents creating terms with naming collisions, hence "safe".
   * For now want to throw an exception whenever there is a collision, but later it may make sense to automatically
   * apply the Barendregt convention to bound variables.
   *
   * @param v1     Term
   * @param v2     Term
   * @param isFree True if comparing free variables, false if comparing bound
   * @return Set of all unique elements of two terms, unless there is a term in common.
   * @throws RuntimeException if the terms have a variable in common
   */
  private def safeUnion(v1: Term, v2: Term, isFree: Boolean): Set[String] = {
    val boundOrFree1 = if (isFree) v1.free() else v1.bound()
    val boundOrFree2 = if (isFree) v2.free() else v2.bound()
    val intersection = boundOrFree1 intersect boundOrFree2
    if (!(intersection.isEmpty)) {
      val varType = if (isFree) "Free" else "Bound"
      throw new RuntimeException(s"$varType variable(s) $intersection occurred in both $v1 and $v2")
    }
    boundOrFree1 ++ boundOrFree2
  }

  def safeFreeUnion(v1: Term, v2: Term) = safeUnion(v1, v2, true)

  def safeBoundUnion(v1: Term, v2: Term) = safeUnion(v1, v2, false)
}

case class Var(val varName: String) extends Term {
  override def toString: String = varName

  override def free(): Set[String] = Set(varName)

  override def bound(): Set[String] = Set()
}

case class App(t1: Term, t2: Term) extends Term {
  override def toString: String = t1.toString + t2.toString

  override def free(): Set[String] = Term.safeFreeUnion(t1, t2)

  override def bound(): Set[String] = Term.safeBoundUnion(t1, t2)
}

case class Abst(t1: Term, t2: Var) extends Term {
  override def toString: String = Term.AbstOp + t2.toString + Term.AbstSep + t1.toString

  override def free(): Set[String] = Term.safeFreeUnion(t1, t2) - t2.varName

  override def bound(): Set[String] = Term.safeBoundUnion(t1, t2) + t2.varName
}



