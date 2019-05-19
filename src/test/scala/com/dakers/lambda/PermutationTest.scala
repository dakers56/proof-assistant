package com.dakers.lambda

import com.dakers.lambda.stlc.{Judgement, Permutation}
import org.scalatest.{FlatSpec, Matchers}

class PermutationTest extends FlatSpec with Matchers with STNotation with UTNotation {

  "If a judgement holds from the empty context, then it" should "hold after permuation" in {
    Permutation(Judgement(SimplyTypedDerivationContext(), "x" :| "X"), SimplyTypedDerivationContext()) should be(Some(stlc.Judgement(SimplyTypedDerivationContext(), "x" :| "X")))
  }

  "If a judgement holds from the a context with one declaration, then it" should "hold after permuation" in {
    stlc.Permutation(stlc.Judgement(SimplyTypedDerivationContext(List("x" :| "X")), "x" :| "X"), SimplyTypedDerivationContext(List("x" :| "X"))) should be(Some(stlc.Judgement(SimplyTypedDerivationContext(List("x" :| "X")), "x" :| "X")))
  }

  "If a judgement holds from the a context with two declarations, then it" should "hold after permuation" in {
    stlc.Permutation(stlc.Judgement(SimplyTypedDerivationContext(List("x" :| "X", "y" :| "Y")), "x" :| "X"), SimplyTypedDerivationContext(List("y" :| "Y", "x" :| "X"))) should be(Some(stlc.Judgement(SimplyTypedDerivationContext(List("y" :| "Y", "x" :| "X")), "x" :| "X")))
    stlc.Permutation(stlc.Judgement(SimplyTypedDerivationContext(List("x" :| "X" ->: "Z".tv, "y" :| "Y")), "x" :| "X"), SimplyTypedDerivationContext(List("y" :| "Y", "x" :| "X" ->: "Z".tv))) should be(Some(stlc.Judgement(SimplyTypedDerivationContext(List("y" :| "Y", "x" :| "X" ->: "Z".tv)), "x" :| "X")))
  }

  "If a judgement holds from the a context with three declarations, then it" should "hold after permuation" in {
    stlc.Permutation(stlc.Judgement(SimplyTypedDerivationContext(List("x" :| "X", "y" :| "Y", "a" :| "A")), "x" :| "X"), SimplyTypedDerivationContext(List("a" :| "A", "y" :| "Y", "x" :| "X"))) should be(Some(stlc.Judgement(SimplyTypedDerivationContext(List("a" :| "A", "y" :| "Y", "x" :| "X")), "x" :| "X")))
    stlc.Permutation(stlc.Judgement(SimplyTypedDerivationContext(List("x" :| "X" ->: "Z".tv, "y" :| "Y", "z" :| "T" ->: "U".tv)), "x" :| "X"), SimplyTypedDerivationContext(List("y" :| "Y", "z" :| "T" ->: "U".tv, "x" :| "X" ->: "Z".tv))) should be(Some(stlc.Judgement(SimplyTypedDerivationContext(List("y" :| "Y", "z" :| "T" ->: "U".tv, "x" :| "X" ->: "Z".tv)), "x" :| "X")))
  }

  "An empty context" should "not be  a permutation of a non-empty context" in {
    stlc.Permutation(stlc.Judgement(SimplyTypedDerivationContext(List("x" :| "X", "y" :| "Y", "a" :| "A")), "x" :| "X"), SimplyTypedDerivationContext(List())) should be(None)
  }

  "A context with an element not in the first" should "not be a permuation of the first" in {
    stlc.Permutation(stlc.Judgement(SimplyTypedDerivationContext(List("x" :| "X", "y" :| "Y", "a" :| "A")), "x" :| "X"), SimplyTypedDerivationContext(List("a" :| "A", "y" :| "Y", "x" :| "X", "z" :| "Z"))) should be(None)
    stlc.Permutation(stlc.Judgement(SimplyTypedDerivationContext(List("x" :| "X", "y" :| "Y", "a" :| "A", "C" :| "D")), "x" :| "X"), SimplyTypedDerivationContext(List("a" :| "A", "y" :| "Y", "x" :| "X", "z" :| "Z"))) should be(None)
  }

}
