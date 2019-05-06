package com.dakers.lambda

/** Represents the grammar
 * Λ = V |(ΛΛ)|(λV . Λ)
 *
 * Nederpelt, Rob. Type Theory and Formal Proof: An Introduction (Kindle Locations 645-646). Cambridge University Press. Kindle Edition.
 */
abstract class Term(val free: Set[String], val bound: Set[String])

object Term {
  val AbstOp = "/|"
  val AbstSep = "."

}

case class Var(val varName: String) extends Term(Set(varName), Set.empty) {
  override def toString: String = varName
}

case class App(t1: Term, t2: Term) extends Term({
  val intersection = t1.free intersect (t2.free)
  if (!intersection.isEmpty) {
    val commonVars = intersection.mkString(",")
    throw new RuntimeException(s"Cannot apply terms with a shared free variable. Variables in common: $commonVars. $Term 1: $t1. Term 2: $t2.")
  }
  t1.free ++ t2.free
},
  {
    val intersection = t1.bound intersect (t2.bound)
    if (!intersection.isEmpty) {
      val commonVars = intersection.mkString(",")
      throw new RuntimeException(s"Cannot apply terms with a shared bound variable. Variables in common: $commonVars. $Term 1: $t1. Term 2: $t2.")
    }
    t1.bound ++ t2.bound
  }
) {
  override def toString: String = t1.toString + t2.toString
}

case class Abst(t1: Term, t2: Var) extends Term(t1.free -- t2.free,
  {
    val intersection = t1.bound intersect (t2.bound)
    if (!intersection.isEmpty) {
      val commonVars = intersection.mkString(",")
      throw new RuntimeException(s"Cannot abstract over term with bound variable. Variables in common: $commonVars. $Term 1: $t1. Term 2: $t2.")
    }
    t1.bound + t2.varName
  }) {

  override def toString: String = Term.AbstOp + t2.toString + Term.AbstSep + t1.toString

}



