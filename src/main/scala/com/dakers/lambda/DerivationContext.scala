package com.dakers.lambda

import scala.collection.mutable.Set


abstract class DerivationContext[T <: Term](val varNames: Set[String] = Set()) extends UTTermNotation {

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
    this
  }

  def +(s: String): DerivationContext[T] = {
    varNames ++= newVars(Var(s))
    this
  }

  def -(t: T): DerivationContext[T] = {
    varNames --= t.bound union t.free
    this
  }

  def -(s: String): DerivationContext[T] = {
    varNames -= s
    this
  }

}

case class UntypedContext(override val varNames: Set[String] = Set()) extends DerivationContext

case class SimplyTypedContext(override val varNames: Set[String] = Set()) extends DerivationContext
