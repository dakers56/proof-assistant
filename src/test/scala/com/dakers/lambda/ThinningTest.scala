package com.dakers.lambda

import com.dakers.lambda.stlc.{Judgement, STStatement, Thinning}
import org.scalatest.{FlatSpec, Matchers}

class ThinningTest extends FlatSpec with Matchers with STNotation with UTNotation {

  "A context " should " always be able to be thinned to itself" in {
    var c = SimplyTypedDerivationContext()
    var j: Judgement[STStatement] = Judgement(c, "x" :| "X")
    Thinning(j, c) should be(Some(j))

    c = SimplyTypedDerivationContext(List("x" :| "X"))
    j = stlc.Judgement(c, "x" :| "X")
    stlc.Thinning(j, c) should be(Some(j))

    c = SimplyTypedDerivationContext(List("x" :| "X", "y" :| "Y"))
    j = stlc.Judgement(c, "z" :| "Z")
    stlc.Thinning(j, c) should be(Some(j))
  }

  "A context " should " always be able to be thinned to itself plus one statement" in {
    var c1 = SimplyTypedDerivationContext()
    var c2 = SimplyTypedDerivationContext()
    c2.add("x" :| "X")
    var j: Judgement[STStatement] = stlc.Judgement(c1, "x" :| "X")
    stlc.Thinning(j, c2) should be(Some(stlc.Judgement(c2, j.subject)))

    c1 = SimplyTypedDerivationContext(List("x" :| "X"))
    c2 = SimplyTypedDerivationContext(List("x" :| "X"))
    c2.add("y" :| "Y")

    j = stlc.Judgement(c1, "x" :| "X")
    stlc.Thinning(j, c2) should be(Some(stlc.Judgement(c2, j.subject)))

    c1 = SimplyTypedDerivationContext(List("x" :| "X", "y" :| "Y"))
    c2 = SimplyTypedDerivationContext(List("x" :| "X", "y" :| "Y"))
    c2.add("a" :| "A")

    j = stlc.Judgement(c1, "z" :| "Z")
    stlc.Thinning(j, c2) should be(Some(stlc.Judgement(c2, j.subject)))
  }

  "A context " should " never be able to be thinned to a smaller one" in {
    var c1 = SimplyTypedDerivationContext()
    var c2 = SimplyTypedDerivationContext()
    c2.add("x" :| "X")
    var j: Judgement[STStatement] = stlc.Judgement(c2, "x" :| "X")
    stlc.Thinning(j, c1) should be(None)

    c1 = SimplyTypedDerivationContext(List("x" :| "X"))
    c2 = SimplyTypedDerivationContext(List("x" :| "X"))
    c2.add("y" :| "Y")

    j = stlc.Judgement(c2, "x" :| "X")
    stlc.Thinning(j, c1) should be(None)

    c1 = SimplyTypedDerivationContext(List("x" :| "X", "y" :| "Y"))
    c2 = SimplyTypedDerivationContext(List("x" :| "X", "y" :| "Y"))
    c2.add("a" :| "A")

    j = stlc.Judgement(c2, "z" :| "Z")
    stlc.Thinning(j, c1) should be(None)
  }

}
