package com.dakers.lambda

import org.scalatest.{FlatSpec, Matchers}

class UntypedDerivationContextTest extends FlatSpec with Matchers with UTNotation with STNotation {

  //empty derivation context
  "An empty untyped context" should " have no terms in it" in {
    UntypedDerivationContext().stmts().size should be(0)
  }

  //Adding elements
  "An empty untyped context" should " have one term in it after adding a term to it" in {
    val term: UTTerm = "x"
    val utContext = UntypedDerivationContext()
    utContext.add(term)
    utContext.stmts() should be(List(term))
  }

  "An untyped context with one term in it " should " have two terms in it after a new one is added" in {
    val utContext = UntypedDerivationContext()
    val term1: UTTerm = Var("x")
    val term2: Var = Var("y")
    utContext.add(term1)

    utContext.add(term2)

    utContext.stmts().size should be(2)
  }

  "An untyped context with one term in it " should " have one term in it after the same term is added to it" in {
    val utContext = UntypedDerivationContext()
    val term1: UTTerm = Var("x")
    utContext.add(term1)

    utContext.add(term1)

    utContext.stmts().size should be(1)
  }

  "An untyped context with one term in it " should " have one term in it after the same term is added to it twice" in {
    val utContext = UntypedDerivationContext()
    val term1: UTTerm = Var("x")
    utContext.add(term1)

    utContext.add(term1)
    utContext.add(term1)

    utContext.stmts().size should be(1)
  }

  //Removing terms
  "An empty untyped context" should " have no terms in it after removing a term" in {
    val utContext = UntypedDerivationContext()
    val term1: UTTerm = Var("x")
    utContext.del(term1)
    utContext.stmts().size should be(0)
  }

  "An empty untyped context" should " have no terms in it after removing a term from it twice" in {
    val utContext = UntypedDerivationContext()
    val term1: UTTerm = Var("x")
    utContext.del(term1)
    utContext.del(term1)
    utContext.stmts().size should be(0)
  }

  "An  untyped context with one element" should " have no terms in it after removing a term from it" in {
    val utContext = UntypedDerivationContext()
    val term1: UTTerm = Var("x")
    utContext.add(term1)
    utContext.del(term1)
  }

  "A  untyped context with two elements" should " have one term in it after removing a term from it" in {
    val utContext = UntypedDerivationContext()
    val term1: UTTerm = "s"
    val term2: UTTerm = "t"
    utContext.add(term1)
    utContext.add(term2)
    utContext.del(term1)
    utContext.stmts().size should be(1)
  }

  //Anti-commutativity
  "Adding a term t1 and then t2 to an empty untyped context " should " not be the same as doing that in the reverse order" in {
    val utContext1 = UntypedDerivationContext()
    val term1 : UTTerm = "x"
    val term2 : UTTerm = "y"
    utContext1.add(term1)
    utContext1.add(term2)

    val utContext2 = UntypedDerivationContext()
    utContext2.add(term2)
    utContext2.add(term1)

    utContext1.stmts() should not(be(utContext2.stmts()))
  }

}
