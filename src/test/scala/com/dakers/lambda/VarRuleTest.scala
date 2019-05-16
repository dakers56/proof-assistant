package com.dakers.lambda

import org.scalatest.{FlatSpec, Matchers}

class VarRuleTest extends FlatSpec with Matchers with UTTermNotation with STTermNotation {

  //Positive cases
  "An empty context" should "not beget any judgements" in {
    VarRule(Judgement(SimplyTypedDerivationContext(), Statement("x", "X"))) should be(None)
  }

  "A context containing only a declaration" should " imply that declaration" in {
    val declaration = "x" :| "X"
    VarRule(Judgement(SimplyTypedDerivationContext(List(declaration)), declaration)) should be(Some(Judgement(SimplyTypedDerivationContext(List(declaration)), declaration)))
  }

  "A context containing only two declarations" should " imply the latter declaration" in {
    val declaration1 = "x" :| "X"
    val declaration2 = "y" :| "Y"
    VarRule(Judgement(SimplyTypedDerivationContext(List(declaration1, declaration2)), declaration2)) should be(Some(Judgement(SimplyTypedDerivationContext(List(declaration1, declaration2)), declaration2)))
  }

  //Negative cases

  "A context containing only a declaration" should " not imply another declaration" in {
    val oneDeclaration = "x" :| "X"
    val anotherDeclaration = "x" :| "Y"
    VarRule(Judgement(SimplyTypedDerivationContext(List(oneDeclaration)), anotherDeclaration)) should be(None)
  }


}
