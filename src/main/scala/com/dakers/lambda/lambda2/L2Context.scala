package com.dakers.lambda.lambda2

import com.dakers.lambda.stlc.STStatement
import com.dakers.lambda.{DerivationContext, STNotation, SimplyTypedDerivationContext, UTTerm}

class L2Context extends DerivationContext[L2Statement] {

  override def proj(vars: Set[String]): List[L2Statement] = {
    val pv = projVars(vars)
    stmts().filter(s => s.utTerm.varNames.subsetOf(pv))
  }

  override def varCount(v: String): Int = super.stmts().map(t => t.utTerm.varNames.contains(v)).count(u => u)

  override def add(t: L2Statement): Unit = {
    if (!(t.utTerm.free intersect bound()).isEmpty)
      throw new RuntimeException("Cannot add a term to a context if one of its free terms is already bound. Term free vars: " + t.utTerm.free + "; context free vars: " + free())
    if (!(t.utTerm.bound intersect free).isEmpty)
      throw new RuntimeException("Cannot add a term to a context if one of its free terms is already bound. Term free vars: " + t.utTerm.free + "; context free vars: " + free)
    super.add(t)
  }

  override def free(): Set[String] = stmts().foldLeft(scala.collection.mutable.Set[String]())((acc, i) => acc ++ i.utTerm.free).toSet

  override def bound(): Set[String] = stmts().foldLeft(scala.collection.mutable.Set[String]())((acc, i) => acc ++ i.utTerm.bound).toSet
}

object L2Context extends STNotation {
  def apply(): L2Context = new L2Context()

  def apply(stmts: List[L2Statement]): L2Context = {
    val ctx = L2Context()
    stmts.foreach(t => ctx.add(t))
    ctx
  }


}
