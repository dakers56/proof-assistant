package com.dakers.lambda

import com.dakers.lambda.stlc.VarRule.ApplRule
import com.dakers.lambda.stlc.{Judgement, STStatement}
import org.scalatest.{FlatSpec, Matchers}

class ApplRuleTest extends FlatSpec with Matchers with UTNotation with STNotation {

  //Positive cases

  "If x : X -> Y and y : X, ApplRule(|- x: X, |- y : X -> Y" should "be of type Y" in {
    val st1: STStatement = "x" :| "S".tv ->: "T".tv
    val st2 = "y" :| "S"
    val ctx = SimplyTypedDerivationContext(List(st1, st2))
    ApplRule(Judgement(ctx, st1), stlc.Judgement(ctx, st2)) should be(Some(stlc.Judgement(ctx, "x" * "y" :| "T")))
  }

  // Variable to a function type
  "If x : S -> T -> U and y:S , ApplRule(|- x: S -> T -> U, |- y : S " should "be of type T -> U" in {
    val st1: STStatement = "x" :| "S".tv ->: "T".tv ->: "U".tv
    val st2 = "y" :| "S"
    val ctx = SimplyTypedDerivationContext(List(st1, st2))
    ApplRule(stlc.Judgement(ctx, st1), stlc.Judgement(ctx, st2)) should be(Some(stlc.Judgement(ctx, "x" * "y" :| "T" ->: "U".tv)))
  }

  // Associativity: variable type to a function type
  "If x: (S -> T) -> U and y: S -> T , ApplRule(|- x: (S -> T) -> U, |- y : S -> T " should "be of type U" in {
    val st1: STStatement = "x" :| ("S".tv ->: "T".tv) ->: "U".tv
    val st2 = "y" :| "S" ->: "T".tv
    val ctx = SimplyTypedDerivationContext(List(st1, st2))
    ApplRule(stlc.Judgement(ctx, st1), stlc.Judgement(ctx, st2)) should be(Some(stlc.Judgement(ctx, "x" * "y" :| "U")))
  }

  // Associativity: function type to a function type
  "If x: (S -> T) -> U -> V and y: S -> T , ApplRule(|- x: (S -> T) -> U -> V, |- y : S -> T " should "be of type U -> V" in {
    val st1: STStatement = "x" :| ("S".tv ->: "T".tv) ->: "U".tv ->: "V".tv
    val st2 = "y" :| "S" ->: "T".tv
    val ctx = SimplyTypedDerivationContext(List(st1, st2))
    ApplRule(stlc.Judgement(ctx, st1), stlc.Judgement(ctx, st2)) should be(Some(stlc.Judgement(ctx, "x" * "y" :| "U" ->: "V".tv)))
  }

  //Application of  a variable to an abstraction term
  "ApplRule(|- |\\x.x : T->U, |- y : T) " should " imply |- (|\\x.x)y : U" in {
    val st1: STStatement = /|("x", "x") :| "T".tv ->: "U".tv
    val st2 = "y" :| "T"
    val ctx = SimplyTypedDerivationContext(List(st1, st2))
    ApplRule(stlc.Judgement(ctx, st1), stlc.Judgement(ctx, st2)) should be(Some(stlc.Judgement(ctx, /|("x", "x") * "y" :| "U")))
  }

  //Application of  an abstraction term to a variable term
  "ApplRule(|- |\\x.x : S->T->U, |- y : S ) " should " imply |- (|\\x.x)y : T -> U" in {
    val st1: STStatement = /|("x", "x") :| "S" ->: "T" ->: "U".tv
    val st2 = "y" :| "S"
    val ctx = SimplyTypedDerivationContext(List(st1, st2))
    ApplRule(stlc.Judgement(ctx, st1), stlc.Judgement(ctx, st2)) should be(Some(stlc.Judgement(ctx, /|("x", "x") * "y" :| "T".tv ->: "U".tv)))
  }

  //Negative cases

  "Applying a variable to a variable " should " fail" in {
    val st1: STStatement = "x" :| "T"
    val st2 = "y" :| "S"
    val ctx = SimplyTypedDerivationContext(List(st1, st2))
    ApplRule(stlc.Judgement(ctx, st1), stlc.Judgement(ctx, st2)) should be(None)
  }

  "Applying x : T to y : S -> T " should " fail" in {
    val st1: STStatement = "x" :| "T"
    val st2 = "y" :| "S" ->: "T".tv
    val ctx = SimplyTypedDerivationContext(List(st1, st2))
    ApplRule(stlc.Judgement(ctx, st1), stlc.Judgement(ctx, st2)) should be(None)
  }

  "Different contexts and a valid application xy where x : S -> T, y: S" should "fail" in {
    val st1: STStatement = "x" :| "T"
    val st2 = "y" :| "S" ->: "T".tv
    val st3 = "z" :| "S" ->: "T".tv
    val ctx1 = SimplyTypedDerivationContext(List(st1, st2))
    val ctx2 = SimplyTypedDerivationContext(List(st1, st2, st3))
    ApplRule(stlc.Judgement(ctx1, st1), stlc.Judgement(ctx2, st2)) should be(None)
  }


}
