package com.dakers.lambda

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable.{ListBuffer, Set}

class DerivationContextTest extends FlatSpec with Matchers with STTermNotation with UTTermNotation {

  //Basic tests for empty contexts
  "An empty untyped context" should " have no variable names and no statements in it" in {
    UntypedContext().varNames should be(empty)
    UntypedContext(Set()).varNames should be(empty)
    UntypedContext(Set(), ListBuffer()).varNames should be(empty)
    UntypedContext(statement = ListBuffer()).varNames should be(empty)
  }

  "An empty simply typed context" should " have no variable names and no statements in it" in {
    SimplyTypedContext().varNames should be(empty)
    SimplyTypedContext(Set()).varNames should be(empty)
    SimplyTypedContext(Set(), ListBuffer()).varNames should be(empty)
    SimplyTypedContext(statement = ListBuffer()).varNames should be(empty)
  }

  //Adding an element to an empty context
  "An empty untyped context" should " have one variable names and one statements in it after a statement is added" in {
    val context: DerivationContext[UTTerm] = UntypedContext()
    val varName = "x"
    context + varName
    context.varNames should be(Set(varName))
    context.statement should be(ListBuffer(Var(varName)))
  }

  "An empty typed context" should " have one variable names and one statements in it after a statement is added" in {
    val context = SimplyTypedContext()
    val varName = "x"
    context + varName
    context.varNames should be(Set(varName))
    context.statement should be(ListBuffer(Var(varName)))
  }

  //Adding an element to a non-empty context
  "An untyped context with one variable and one statement" should " have two variable names and two statements in it after a statement is added" in {
    val context: DerivationContext[UTTerm] = UntypedContext()
    val varName1 = "x"
    context + varName1

    val varName2 = "y"
    context + varName2
    context.varNames should be(Set(varName1, varName2))
    context.statement should be(ListBuffer(Var(varName1), Var(varName2)))
  }

  "A typed context with one variable and one statement" should " have two variable names and two statements in it after a statement is added" in {
    val context = SimplyTypedContext()
    val varName1 = "x"
    context + varName1

    val varName2 = "y"
    context + varName2
    context.varNames should be(Set(varName1, varName2))
    context.statement should be(ListBuffer(Var(varName1), Var(varName2)))
  }

}
