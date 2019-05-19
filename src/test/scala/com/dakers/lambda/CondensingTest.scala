package com.dakers.lambda

import com.dakers.lambda.stlc.{Condensing, Judgement, STStatement}
import org.scalatest.{FlatSpec, Matchers}


class CondensingTest extends FlatSpec with Matchers with UTNotation with STNotation {

  //Basic tests for empty contexts
  "If a judgement holds with the empty context, " should " it should hold after condensing" in {
    val j: Judgement[STStatement] = Judgement(SimplyTypedDerivationContext(), "x" :| "X")
    j should be(Condensing(j))
  }

  "If a judgement holds with extraneous variables in the context, " should " it should hold after condensing" in {
    var j: Judgement[STStatement] = stlc.Judgement(SimplyTypedDerivationContext(List("y" :| "Y", "z" :| "Z")), "x" :| "X")
    stlc.Judgement(SimplyTypedDerivationContext(), "x" :| "X") should be(Condensing(j))
    j = stlc.Judgement(SimplyTypedDerivationContext(List("x" :| "X", "y" :| "Y", "z" :| "Z")), "x" :| "X")
    stlc.Judgement(SimplyTypedDerivationContext(List("x" :| "X")), "x" :| "X") should be(Condensing(j))
  }

}
