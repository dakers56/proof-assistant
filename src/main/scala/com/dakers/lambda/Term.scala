package com.dakers.lambda

/** Represents the grammar
 * Λ = V |(ΛΛ)|(λV . Λ)
 *
 * Nederpelt, Rob. Type Theory and Formal Proof: An Introduction (Kindle Locations 645-646). Cambridge University Press. Kindle Edition.
 */
abstract class Term {
  val boundVar: scala.collection.mutable.Set[Var] = scala.collection.mutable.Set()
  val freeVar: scala.collection.mutable.Set[Var] = scala.collection.mutable.Set()

  def addBound(v: Var): Unit = {
    if (boundVar.contains(v)) {
      throw new RuntimeException("Variable " + v + " was already bound in this term.")
    }
    boundVar += v
  }

  def addFree(v: Var): Unit = {
    if (freeVar.contains(v)) {
      throw new RuntimeException("Variable " + v + " was already free in this term.")
    }
    freeVar += v
  }
}

object Term {
  val abstOp = "/|"
  val abstSep = "."

}

case class Var(val varName: String) extends Term {
  override def toString: String = varName
}

case class App(v1: Term, v2: Term) extends Term {
  override def toString: String = v1.toString + v2.toString
}

case class Abst(v1: Term, v2: Var) extends Term {
  override def toString: String = Term.abstOp + v2.toString + Term.abstSep + v1.toString
}

/**
 * Using companion object to ensure that new variables do not conflict with those already in the term.
 */
object Abst {
  def apply(v1: Term, v2: Var) = {
    val newTerm = new Abst(v1, v2)
    newTerm.addBound(v2)
    newTerm
  }
}



