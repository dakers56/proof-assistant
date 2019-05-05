package com.dakers.lambda

/** Represents the grammar
 * Λ = V |(ΛΛ)|(λV . Λ)
 *
 * Nederpelt, Rob. Type Theory and Formal Proof: An Introduction (Kindle Locations 645-646). Cambridge University Press. Kindle Edition.
 */
abstract class Term

object Term {
  val abstOp = "/|"
  val abstSep = "."
}

abstract class Var(val varName: String) extends Term{
  override def toString: String = varName
}

object Var {
  def unapply(arg: Var): Option[String] = Some(arg.varName)
}

case class App(v1: Term, v2: Term) extends Term {
  override def toString: String = v1.toString + v2.toString
}

case class Abst(v1: Term, v2: BVar) extends Term {
  override def toString: String = Term.abstOp + v2.toString + Term.abstSep + v1.toString
}

case class BVar(bVarName: String) extends Var(bVarName)

case class FVar(fVarName: String) extends Var(fVarName)


