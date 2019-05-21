package com.dakers.lambda.lambda2

import com.dakers.lambda.stlc.VarType
import com.dakers.lambda.{STNotation, UTNotation, Var}
import org.scalatest.{FlatSpec, Matchers}

class L2ContextTest extends FlatSpec with Matchers with STNotation with UTNotation with L2Notation {

  //empty derivation context
  "An empty typed context" should " have no terms in it" in {
    val c: L2Context = L2Context(Nil, Nil)
    c.typeContext.stmts().size should be(0)
    c.stmtContext.stmts().size should be(0)
  }

  //Adding elements
  "An empty typed context" should " have one term in it after adding a term to it" in {
    val stContext = L2Context()
    val term: L2Statement = L2Statement(Var("x"), VarType("x"))
    stContext.add(term)
    stContext.stmtContext.stmts() should be(List(term))
    stContext.typeContext.stmts() should be(empty)
  }

  "An typed context with one term in it " should " have two terms in it after a new one is added" in {
    val stContext = L2Context()
    val term1: L2Statement = L2Statement("x", "y".tv)
    stContext.add(term1)

    val term2: L2Statement = L2Statement("y", "y".tv)
    stContext.add(term2)

    stContext.stmtContext.stmts().size should be(2)
    stContext.typeContext.stmts().size should be(0)
  }

  "A typed context with one term in it " should " have one term in it after the same term is added to it" in {
    val stContext = L2Context()
    val term1: L2Statement = "x" :|| "y"
    stContext.add(term1)
    stContext.add(term1)

    stContext.stmtContext.stmts().size should be(1)
  }

  "A typed context with one term in it " should " have one term in it after the same term is added to it twice" in {
    val stContext = L2Context()
    val term1: L2Statement = "x" :|| "y"
    stContext.add(term1)

    stContext.add(term1)
    stContext.add(term1)

    stContext.stmtContext.stmts().size should be(1)
    stContext.typeContext.stmts().size should be(0)
  }

  //Removing terms
  "An empty typed context" should " have no terms in it after removing a term" in {
    val stContext = L2Context()
    val term1: L2Statement = "s" :|| "T"
    stContext.del(term1)
    stContext.stmtContext.stmts().size should be(0)
    stContext.typeContext.stmts().size should be(0)
  }

  "An empty typed context" should " have no terms in it after removing a term from it twice" in {
    val stContext = L2Context()
    val term1: L2Statement = "s" :|| "T"
    stContext.del(term1)
    stContext.del(term1)
    stContext.stmtContext.stmts().size should be(0)
    stContext.typeContext.stmts().size should be(0)
  }

  "A  typed context with one element" should " have no terms in it after removing a term from it" in {
    val stContext = L2Context()
    val term1: L2Statement = "s" :|| "T"
    stContext.add(term1)
    stContext.del(term1)
    stContext.stmtContext.stmts().size should be(0)
    stContext.typeContext.stmts().size should be(0)
  }

  "A  typed context with two elements" should " have one term in it after removing a term from it" in {
    val stContext = L2Context()
    val term1: L2Statement = "s" :|| "T"
    val term2: L2Statement = "t" :|| "U"
    stContext.add(term1)
    stContext.add(term2)
    stContext.del(term1)
    stContext.stmtContext.stmts().size should be(1)
    stContext.typeContext.stmts().size should be(0)
  }

  //Anti-commutativity
  "Adding a term t1 and then t2 to an empty typed context " should " not be the same as doing that in the reverse order" in {
    val stContext1 = L2Context()
    val term1: L2Statement = "x" :|| "X"
    val term2: L2Statement = "y" :|| "Y"
    stContext1.add(term1)
    stContext1.add(term2)

    val stContext2 = L2Context()
    stContext2.add(term2)
    stContext2.add(term1)

    stContext1.stmtContext.stmts() should not(be(stContext2.stmtContext.stmts()))
  }

  "A context with no free variables' free() method" should "return an empty set" in {
    L2Context().free() should be(Set())
    L2Context(List(/|("x", "x") :|| "T"), List()).free() should be(Set())
    L2Context(List(/|("x", "x") :|| "T", /|("y", "y") :|| "T"), List()).free() should be(Set())
  }

  "A context with one free variables' free() method" should "return a set containing only that variable" in {
    L2Context().free() should be(Set())
    L2Context(List("x" :|| "T"), List()).free() should be(Set("x"))
    L2Context(List(/|("x", "x") :|| "T", "y" :|| "T"), List()).free() should be(Set("y"))
    L2Context(List(/|("x", "x") :|| "T", "y" :|| "T", "z" :|| "T"), List()).free() should be(Set("y", "z"))
  }

  "Adding a term with a free variable that is already bound in the context" should "throw an exception" in {
    val ctx = L2Context(List(/|("x", "x") :|| "T"), List())
    intercept[RuntimeException](
      ctx.add("x" :|| "T")
    )

    intercept[RuntimeException](
      ctx.add(("x" * "y") :|| "T")
    )
  }

  "The domain of the empty context" should "be empty" in {
    L2Context().dom() should be(Set())
  }

  "The domain of x:X " should "be x" in {
    L2Context(List("x" :|| "X"), List()).dom() should be(Set("x"))
  }

  "The domain of x:X, y:Y " should "be x" in {
    L2Context(List("x" :|| "X", "y" :|| "Y"), List()).dom() should be(Set("x", "y"))
  }

  "The empty context" should "equal itself" in {
    L2Context(List(), List()) should be(L2Context(List(), List()))
  }

}
