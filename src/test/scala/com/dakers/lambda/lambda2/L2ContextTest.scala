package com.dakers.lambda.lambda2

import com.dakers.lambda.SimplyTypedDerivationContext.L2Context
import com.dakers.lambda.{L2Conv, STNotation, UTNotation, UTTerm, Var, stlc}
import com.dakers.lambda.stlc.{STStatement, VarType}
import org.scalatest.{FlatSpec, Matchers}

class L2ContextTest extends FlatSpec with Matchers with UTNotation with STNotation with L2Conv {

//  //empty derivation context
//  "An empty typed context" should " have no terms in it" in {
//    L2Context().stmts().size should be(0)
//  }
//
//  //Adding elements
//  "An empty typed context" should " have one term in it after adding a term to it" in {
//    val stContext = L2Context()
//    val term: UTTerm = "x"
//    val typ = π("x", "X")
//    stContext.add(L2Statement())
//    stContext.stmts should be(List(term))
//  }
//
//  "An typed context with one term in it " should " have two terms in it after a new one is added" in {
//    val stContext = L2Context()
//    val term1: STStatement = stlc.STStatement("x", "y".tv)
//    stContext.add(term1)
//
//    val term2: STStatement = stlc.STStatement("y", "y".tv)
//    stContext.add(term2)
//
//    stContext.stmts().size should be(2)
//  }
//
//  "A typed context with one term in it " should " have one term in it after the same term is added to it" in {
//    val stContext = L2Context()
//    val term1: STStatement = "x" :| "y"
//    stContext.add(term1)
//    stContext.add(term1)
//
//    stContext.stmts().size should be(1)
//  }
//
//  "A typed context with one term in it " should " have one term in it after the same term is added to it twice" in {
//    val stContext = L2Context()
//    val term1: STStatement = "x" :| "y"
//    stContext.add(term1)
//
//    stContext.add(term1)
//    stContext.add(term1)
//
//    stContext.stmts().size should be(1)
//  }

}
