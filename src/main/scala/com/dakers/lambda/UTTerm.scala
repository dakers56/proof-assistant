package com.dakers.lambda


/** Represents the grammar
 * Λ = V |(ΛΛ)|(λV . Λ)
 *
 * Nederpelt, Rob. Type Theory and Formal Proof: An Introduction (Kindle Locations 645-646). Cambridge University Press. Kindle Edition.
 */
abstract class UTTerm(val free: Set[String], val bound: Set[String]) {
  val varNames = free union bound

}

object UTTerm {
  def rename(s: String) = Var(s + "1")

  def tryRename(a1: App, a2: App) = {
    while (true) {
      try {
        val result = App(a1, a2)
      }
      catch {
        case ex: AlreadyBoundException => {
          val intersection = a1.varNames intersect (a2.varNames)
          val renamed = intersection.map(x => rename(x))
        }
      }
    }
  }
}

/**
 * Represents a variable in the lambda calculus.
 *
 * @param varName Name of the variable
 */
case class Var(val varName: String) extends UTTerm(Set(varName), Set.empty) {
  override def toString: String = varName

}


/**
 * Represents the application of one lambda term to another.
 *
 * @param t1 M in an application MN.
 * @param t2 N in an application MN.
 */
case class App(t1: UTTerm, t2: UTTerm) extends UTTerm({
  t1.free ++ t2.free
},
  {
    val intersection = t1.bound intersect (t2.bound)
    if (!intersection.isEmpty) {
      val commonVars = intersection.mkString(",")
      throw new AlreadyBoundException(commonVars, t1, t2)
    }
    t1.bound ++ t2.bound
  }
) {
  override def toString: String = "(" + t1.toString + t2.toString + ")"
}

/**
 * Represents abstraction in the lambda calculus.
 *
 * @param t1 Term to abstract over.
 * @param t2 Variable to bind.
 */
case class Abst(t1: UTTerm, t2: Var) extends UTTerm(t1.free -- t2.free,
  {
    if (t1.bound(t2.varName)) {
      throw new AlreadyBoundException(t2.varName, t1, t1)
    }
    t1.bound + t2.varName
  }) {

  override def toString: String = AbstOp + t2.toString + AbstSep + t1.toString

}

case class AlreadyBoundException(v: String, t1: UTTerm, t2: UTTerm) extends RuntimeException(s"Variable $v from $t1 was already bound in $t2")


