package com.dakers.lambda.lambda2

import com.dakers.lambda.SimplyTypedDerivationContext.L2Context
import com.dakers.lambda.{L2Conv, STNotation, UTNotation, UTTerm, Var, stlc}
import com.dakers.lambda.stlc.{STStatement, VarType}
import org.scalatest.{FlatSpec, Matchers}

class L2ContextTest extends FlatSpec with Matchers with UTNotation with STNotation with L2Conv {

  //empty derivation context
  "An empty typed context" should " have no terms in it" in {
    L2Context().stmts().size should be(0)
  }

  //Adding elements
  "An empty typed context" should " have one term in it after adding a term to it" in {
    val stContext = L2Context()
    val term: STStatement = "x" :| "X"
    val typ = π(term, "X")
    stContext.add(L2Statement(term, typ))
    stContext.stmts should be(List(L2Statement(term, typ)))
  }

  "An typed context with one term in it " should " have two terms in it after a new one is added" in {
    val stContext = L2Context()
    val term1: STStatement = stlc.STStatement("x", "y".tv)
    stContext.add(L2Statement(term1, "X"))

    val term2: STStatement = stlc.STStatement("y", "y".tv)
    stContext.add(L2Statement(term2, "X"))

    stContext.stmts().size should be(2)
  }

  "A typed context with one term in it " should " have one term in it after the same term is added to it" in {
    val stContext = L2Context()
    val term1: STStatement = "x" :| "y"
    stContext.add(L2Statement(term1, "X"))
    stContext.add(L2Statement(term1, "X"))

    stContext.stmts().size should be(1)
  }

  "A typed context with one term in it " should " have one term in it after the same term is added to it twice" in {
    val stContext = L2Context()
    val term1: STStatement = "x" :| "y"
    stContext.add(L2Statement(term1, "X"))

    stContext.add(L2Statement(term1, "X"))
    stContext.add(L2Statement(term1, "X"))

    stContext.stmts().size should be(1)
  }

}
