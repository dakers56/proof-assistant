package com.dakers.lambda

import org.scalatest.{FlatSpec, Matchers}

class TypedDerivationContextTest extends FlatSpec with Matchers with UTNotation with STNotation {

  //empty derivation context
  "An empty typed context" should " have no terms in it" in {
    SimplyTypedDerivationContext().stmts().size should be(0)
  }

  //Adding elements
  "An empty typed context" should " have one term in it after adding a term to it" in {
    val stContext = SimplyTypedDerivationContext()
    val term: Statement = Statement(Var("x"), VarType("x"))
    stContext.add(term)
    stContext.stmts should be(List(term))
  }

  "An typed context with one term in it " should " have two terms in it after a new one is added" in {
    val stContext = SimplyTypedDerivationContext()
    val term1: Statement = Statement("x", "y".tv)
    stContext.add(term1)

    val term2: Statement = Statement("y", "y".tv)
    stContext.add(term2)

    stContext.stmts().size should be(2)
  }

  "A typed context with one term in it " should " have one term in it after the same term is added to it" in {
    val stContext = SimplyTypedDerivationContext()
    val term1: Statement = "x" :| "y"
    stContext.add(term1)
    stContext.add(term1)

    stContext.stmts().size should be(1)
  }

  "A typed context with one term in it " should " have one term in it after the same term is added to it twice" in {
    val stContext = SimplyTypedDerivationContext()
    val term1: Statement = "x" :| "y"
    stContext.add(term1)

    stContext.add(term1)
    stContext.add(term1)

    stContext.stmts().size should be(1)
  }

  //Removing terms
  "An empty typed context" should " have no terms in it after removing a term" in {
    val stContext = SimplyTypedDerivationContext()
    val term1: Statement = "s" :| "T"
    stContext.del(term1)
    stContext.stmts().size should be(0)
  }

  "An empty typed context" should " have no terms in it after removing a term from it twice" in {
    val stContext = SimplyTypedDerivationContext()
    val term1: Statement = "s" :| "T"
    stContext.del(term1)
    stContext.del(term1)
    stContext.stmts().size should be(0)
  }

  "A  typed context with one element" should " have no terms in it after removing a term from it" in {
    val stContext = SimplyTypedDerivationContext()
    val term1: Statement = "s" :| "T"
    stContext.add(term1)
    stContext.del(term1)
    stContext.stmts().size should be(0)
  }

  "A  typed context with two elements" should " have one term in it after removing a term from it" in {
    val stContext = SimplyTypedDerivationContext()
    val term1: Statement = "s" :| "T"
    val term2: Statement = "t" :| "U"
    stContext.add(term1)
    stContext.add(term2)
    stContext.del(term1)
    stContext.stmts().size should be(1)
  }

  //Anti-commutativity
  "Adding a term t1 and then t2 to an empty typed context " should " not be the same as doing that in the reverse order" in {
    val stContext1 = SimplyTypedDerivationContext()
    val term1 : Statement = "x" :| "X"
    val term2 : Statement = "y" :| "Y"
    stContext1.add(term1)
    stContext1.add(term2)

    val stContext2 = SimplyTypedDerivationContext()
    stContext2.add(term2)
    stContext2.add(term1)

    stContext1.stmts() should not(be(stContext2.stmts()))
  }

}
