package com.dakers.lambda

import org.scalatest.{FlatSpec, Matchers}


class CondensingTest extends FlatSpec with Matchers with UTNotation with STNotation {

  //Basic tests for empty contexts
  "If a judgement holds with the empty context, " should " it should hold after condensing" in {
    val j: Judgement[Statement] = Judgement(SimplyTypedDerivationContext(), "x" :| "X")
    j should be(Condensing(j))
  }

  "If a judgement holds with extraneous variables in the context, " should " it should hold after condensing" in {
    var j: Judgement[Statement] = Judgement(SimplyTypedDerivationContext(List("y" :| "Y", "z" :| "Z")), "x" :| "X")
    Judgement(SimplyTypedDerivationContext(), "x" :| "X") should be(Condensing(j))
    j = Judgement(SimplyTypedDerivationContext(List("x" :| "X", "y" :| "Y", "z" :| "Z")), "x" :| "X")
    Judgement(SimplyTypedDerivationContext(List("x" :| "X")), "x" :| "X") should be(Condensing(j))
  }

}
