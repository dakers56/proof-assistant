package com.dakers.lambda.lambda2

import com.dakers.lambda.stlc.VarType
import com.dakers.lambda.{STNotation, UTNotation, Var}
import org.scalatest.{FlatSpec, Matchers}

class L2ContextTest extends FlatSpec with Matchers with UTNotation with STNotation with L2Notation {

  //empty derivation context
  "An empty typed context" should " have no terms in it" in {
    L2Context().stmts().size should be(0)
  }

  //Adding elements
  "An empty typed context" should " have one term in it after adding a term to it" in {
    val stContext = L2Context()
    val term: L2Statement = L2Statement(Var("x"), VarType("x"))
    stContext.add(term)
    stContext.stmts should be(List(term))
  }

  "An typed context with one term in it " should " have two terms in it after a new one is added" in {
    val stContext = L2Context()
    val term1: L2Statement = L2Statement("x", "y".tv)
    stContext.add(term1)

    val term2: L2Statement = L2Statement("y", "y".tv)
    stContext.add(term2)

    stContext.stmts().size should be(2)
  }

  "A typed context with one term in it " should " have one term in it after the same term is added to it" in {
    val stContext = L2Context()
    val term1: L2Statement = "x" :|| "y"
    stContext.add(term1)
    stContext.add(term1)

    stContext.stmts().size should be(1)
  }

  "A typed context with one term in it " should " have one term in it after the same term is added to it twice" in {
    val stContext = L2Context()
    val term1: L2Statement = "x" :|| "y"
    stContext.add(term1)

    stContext.add(term1)
    stContext.add(term1)

    stContext.stmts().size should be(1)
  }

  //Removing terms
  "An empty typed context" should " have no terms in it after removing a term" in {
    val stContext = L2Context()
    val term1: L2Statement = "s" :|| "T"
    stContext.del(term1)
    stContext.stmts().size should be(0)
  }

  "An empty typed context" should " have no terms in it after removing a term from it twice" in {
    val stContext = L2Context()
    val term1: L2Statement = "s" :|| "T"
    stContext.del(term1)
    stContext.del(term1)
    stContext.stmts().size should be(0)
  }

  "A  typed context with one element" should " have no terms in it after removing a term from it" in {
    val stContext = L2Context()
    val term1: L2Statement = "s" :|| "T"
    stContext.add(term1)
    stContext.del(term1)
    stContext.stmts().size should be(0)
  }

  "A  typed context with two elements" should " have one term in it after removing a term from it" in {
    val stContext = L2Context()
    val term1: L2Statement = "s" :|| "T"
    val term2: L2Statement = "t" :|| "U"
    stContext.add(term1)
    stContext.add(term2)
    stContext.del(term1)
    stContext.stmts().size should be(1)
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

    stContext1.stmts() should not(be(stContext2.stmts()))
  }

  "A context with no free variables' free() method" should "return an empty set" in {
    L2Context().free() should be(Set())
    L2Context(List(/|("x", "x") :|| "T")).free() should be(Set())
    L2Context(List(/|("x", "x") :|| "T", /|("y", "y") :|| "T")).free() should be(Set())
  }

  "A context with one free variables' free() method" should "return a set containing only that variable" in {
    L2Context().free() should be(Set())
    L2Context(List("x" :|| "T")).free() should be(Set("x"))
    L2Context(List(/|("x", "x") :|| "T", "y" :|| "T")).free() should be(Set("y"))
    L2Context(List(/|("x", "x") :|| "T", "y" :|| "T", "z" :|| "T")).free() should be(Set("y", "z"))
  }

  "Adding a term with a free variable that is already bound in the context" should "throw an exception" in {
    val ctx = L2Context(List(/|("x", "x") :|| "T"))
    intercept[RuntimeException](
      ctx.add("x" :|| "T")
    )

    intercept[RuntimeException](
      ctx.add(("x" * "y") :|| "T")
    )
  }

  "Adding a term with a bound variable that is already fre in the context" should "throw an exception" in {
    val ctx = L2Context(List("x" :|| "T"))
    intercept[RuntimeException](
      ctx.add(/|("x", "x") :|| "T")
    )

    intercept[RuntimeException](
      ctx.add(/|("x", "x" * "y") :|| "T")
    )
  }

  "The domain of the empty context" should "be empty" in {
    L2Context().dom() should be(Set())
  }

  "The domain of x:X " should "be x" in {
    L2Context(List("x" :|| "X")).dom() should be(Set("x"))
  }

  "The domain of x:X, y:Y " should "be x" in {
    L2Context(List("x" :|| "X", "y" :|| "Y")).dom() should be(Set("x", "y"))
  }

  "The projection of the empty context onto the empty context" should "be empty" in {
    L2Context(List()).projVars(Set()) should be(Set())
  }

  "The projection of the empty context onto a non-empty context" should "be empty" in {
    L2Context(List()).projVars(Set("x", "y", "z")) should be(Set())
  }

  "The projection of x:X, y:Y onto x : X" should "be x" in {
    L2Context(List("x" :|| "X", "y" :|| "Y")).projVars(Set("x")) should be(Set("x"))
    L2Context(List("x" :|| "X", "y" :|| "Y")).proj(Set("x")) should be(List("x" :|| "X"))
  }

  "The projection of of x:X, y:Y, z :|| Z onto x, z" should "be x :|| X, z :|| Z" in {
    L2Context(List("x" :|| "X", "y" :|| "Y", "z" :|| "Z")).proj(Set("x", "z")) should be(List("x" :|| "X", "z" :|| "Z"))
  }

  "A context" should "always be a permutation of itself" in {
    L2Context().isPerm(L2Context()) should be(true)
    L2Context(List("x" :|| "X")).isPerm(L2Context(List("x" :|| "X"))) should be(true)
    L2Context(List("x" :|| "X", "y" :|| "Y")).isPerm(L2Context(List("x" :|| "X", "y" :|| "Y"))) should be(true)
  }

  "A context with the same declarations in a different order as the original" should "always be a permutation of the original" in {
    L2Context(List("x" :|| "X", "y" :|| "Y")).isPerm(L2Context(List("y" :|| "Y", "x" :|| "X"))) should be(true)
    L2Context(List("x" :|| "X", "y" :|| "Y", "z" :|| "Z")).isPerm(L2Context(List("y" :|| "Y", "z" :|| "Z", "x" :|| "X"))) should be(true)
    L2Context(List("x" :|| "X", "y" :|| "Y", "z" :|| "Z")).isPerm(L2Context(List("z" :|| "Z", "y" :|| "Y", "x" :|| "X"))) should be(true)
  }

  "A context with a declaration not in the original" should "never be a permutation of the original" in {
    L2Context(List("x" :|| "X", "y" :|| "Y", "z" :|| "Z")).isPerm(L2Context(List("y" :|| "Y", "x" :|| "X"))) should be(false)
    L2Context(List("x" :|| "X", "y" :|| "Y", "z" :|| "Z", "a" :|| "A")).isPerm(L2Context(List("y" :|| "Y", "z" :|| "Z", "x" :|| "X"))) should be(false)
    L2Context(List("x" :|| "X", "y" :|| "Y", "z" :|| "Z", "a" :|| "A")).isPerm(L2Context(List("z" :|| "Z", "y" :|| "Y", "x" :|| "X"))) should be(false)
  }

}
