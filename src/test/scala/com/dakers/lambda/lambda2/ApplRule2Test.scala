package com.dakers.lambda.lambda2

import com.dakers.lambda.{STNotation, UTNotation}
import org.scalatest.{FlatSpec, Matchers}

class ApplRule2Test extends FlatSpec with Matchers with UTNotation with STNotation with L2Notation {

  "Applying two terms with different contexts" should "return None" in {
    val g1 = L2Context()
    val g2 = L2Context(List(L2Statement("x", "X".tv)), List())
    Appl2(L2Judgement(g1, L2Statement("x", "x")), L2Judgement(g2, L2Statement("x", "x"))) should be(None)
  }

  "If the first term is not a pi type, Appl2" should "return None" in {
    val g1 = L2Context(List(L2Statement("x", "X".tv)))
    Appl2(L2Judgement(g1, L2Statement("x", "x")), L2Judgement(g1, L2Statement("x", "x"))) should be(None)
  }

  "Given |- (|^|A:*) (/|x:A)x[A:=B]" should "return Some(/|x:B)" in {
    val ut1 = /|("x", "x")
    val ut2 = "y"
    val stmt1 = L2Statement(ut1, π("x", "x".tv))
    val stmt2 = L2Statement(ut2, "T1".tv)
    val g = L2Context(List())
    Appl2(L2Judgement(g, stmt1), L2Judgement(g, stmt2)) should be(Some(L2Judgement(g, L2Statement(ut1 * ut2, "T1".tv))))
  }

  "Given |- (|^|A:*) (/|x:A)xy[A:=B->C]" should "return Some(/|x x:B->C)" in {
    val ut1 = /|("x", "x")
    val ut2 = "y"
    val stmt1 = L2Statement(ut1, π("x", "x".tv))
    val stmt2 = L2Statement(ut2, ArrType2("B", "C"))
    val g = L2Context(List())
    Appl2(L2Judgement(g, stmt1), L2Judgement(g, stmt2)) should be(Some(L2Judgement(g, L2Statement(ut1 * ut2, ArrType2("B", "C")))))
  }

  "Given |- (|^|A:*) (/|x:A)x[A:=π(b, b)]" should "return Some(/|x x:B->C)" in {
    val ut1 = /|("x", "x")
    val ut2 = "y"
    val stmt1 = L2Statement(ut1, π("x", "x".tv))
    val piB = π("b", "b".tv)
    val stmt2 = L2Statement(ut2, piB)
    val g = L2Context(List())
    Appl2(L2Judgement(g, stmt1), L2Judgement(g, stmt2)) should be(Some(L2Judgement(g, L2Statement(ut1 * ut2, piB))))
  }

}
