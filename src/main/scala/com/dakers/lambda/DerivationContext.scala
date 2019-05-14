package com.dakers.lambda

import com.dakers.lambda.UTTermNotation.strToVar

import scala.collection.mutable.Set


abstract class DerivationContext(val varNames: Set[String] = Set()) extends UTTermNotation {

  private def newVars(t: UTTerm): scala.collection.Set[String] = {
    val tVars = t.bound union t.free
    val intersect = tVars intersect varNames
    if (!intersect.isEmpty) {
      throw new RuntimeException(s"Term $t contained variable(s) already used in this context: $intersect")
    }
    tVars
  }

  def +(t: UTTerm): DerivationContext = {
    varNames ++= newVars(t)
    this
  }

  def +(s: String): DerivationContext = {
    this + strToVar(s)
  }

  def -(t: UTTerm): DerivationContext = {
    varNames --= t.bound union t.free
    this
  }

  def -(s: String): DerivationContext = {
    this - strToVar(s)
  }

}

case class UntypedContext(override val varNames: Set[String] = Set()) extends DerivationContext

case class SimplyTypedContext(override val varNames: Set[String] = Set()) extends DerivationContext
