package com.dakers.lambda

import scala.collection.mutable.{ListBuffer, Set}


abstract class DerivationContext[T <: Term](val varNames: Set[String] = Set(), val statement: ListBuffer[T] = ListBuffer()) extends UTTermNotation {

  private def newVars(t: Term): scala.collection.Set[String] = {
    val tVars = t.bound union t.free
    val intersect = tVars intersect varNames
    if (!intersect.isEmpty) {
      throw new RuntimeException(s"Term $t contained variable(s) already used in this context: $intersect")
    }
    tVars
  }

  def +(t: T): DerivationContext[T] = {
    varNames ++= newVars(t)
    statement += t
    this
  }

  def +(s: String): DerivationContext[T] = {
    val newVar = Var(s)
    varNames ++= newVars(newVar)
    statement += newVar.asInstanceOf[T]
    this
  }

  def -(t: T): DerivationContext[T] = {
    varNames --= t.bound union t.free
    statement.filter(u => u != t)
    this
  }

  def -(s: String): DerivationContext[T] = {
    varNames -= s
    statement.filter(t => t != Var(s))
    this
  }

}

case class SimplyTypedContext(override val varNames: Set[String] = Set(), override val statement: ListBuffer[STTerm] = new ListBuffer[STTerm]()) extends DerivationContext(varNames, statement)

case class UntypedContext(override val varNames: Set[String] = Set(), override val statement: ListBuffer[UTTerm] = new ListBuffer[UTTerm]()) extends DerivationContext(varNames, statement)

