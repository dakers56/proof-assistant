package com.dakers.lambda

import org.scalatest.{FlatSpec, Matchers}

class ThinningTest extends FlatSpec with Matchers with STNotation with UTNotation {

  "A context " should " always be able to be thinned to itself" in {
    var c = SimplyTypedDerivationContext()
    var j: Judgement[Statement] = Judgement(c, "x" :| "X")
    Thinning(j, c) should be(Some(j))

    c = SimplyTypedDerivationContext(List("x" :| "X"))
    j = Judgement(c, "x" :| "X")
    Thinning(j, c) should be(Some(j))

    c = SimplyTypedDerivationContext(List("x" :| "X", "y" :| "Y"))
    j = Judgement(c, "z" :| "Z")
    Thinning(j, c) should be(Some(j))
  }

  "A context " should " always be able to be thinned to itself plus one statement" in {
    var c1 = SimplyTypedDerivationContext()
    var c2 = SimplyTypedDerivationContext()
    c2.add("x" :| "X")
    var j: Judgement[Statement] = Judgement(c1, "x" :| "X")
    Thinning(j, c2) should be(Some(Judgement(c2, j.subject)))

    c1 = SimplyTypedDerivationContext(List("x" :| "X"))
    c2 = SimplyTypedDerivationContext(List("x" :| "X"))
    c2.add("y" :| "Y")

    j = Judgement(c1, "x" :| "X")
    Thinning(j, c2) should be(Some(Judgement(c2, j.subject)))

    c1 = SimplyTypedDerivationContext(List("x" :| "X", "y" :| "Y"))
    c2 = SimplyTypedDerivationContext(List("x" :| "X", "y" :| "Y"))
    c2.add("a" :| "A")

    j = Judgement(c1, "z" :| "Z")
    Thinning(j, c2) should be(Some(Judgement(c2, j.subject)))
  }

  "A context " should " never be able to be thinned to a smaller one" in {
    var c1 = SimplyTypedDerivationContext()
    var c2 = SimplyTypedDerivationContext()
    c2.add("x" :| "X")
    var j: Judgement[Statement] = Judgement(c2, "x" :| "X")
    Thinning(j, c1) should be(None)

    c1 = SimplyTypedDerivationContext(List("x" :| "X"))
    c2 = SimplyTypedDerivationContext(List("x" :| "X"))
    c2.add("y" :| "Y")

    j = Judgement(c2, "x" :| "X")
    Thinning(j, c1) should be(None)

    c1 = SimplyTypedDerivationContext(List("x" :| "X", "y" :| "Y"))
    c2 = SimplyTypedDerivationContext(List("x" :| "X", "y" :| "Y"))
    c2.add("a" :| "A")

    j = Judgement(c2, "z" :| "Z")
    Thinning(j, c1) should be(None)
  }

}
