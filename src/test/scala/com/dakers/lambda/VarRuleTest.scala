package com.dakers.lambda

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable.ListBuffer

class VarRuleTest extends FlatSpec with Matchers with UTNotation with STNotation {

  //Positive cases

  "A context containing only a declaration" should " imply that declaration" in {
    val declaration = "x" :| "X"
    VarRule(Judgement(SimplyTypedDerivationContext(List(declaration)), declaration)) should be(Some(Judgement(SimplyTypedDerivationContext(List(declaration)), declaration)))
  }

  "A context containing only two declarations" should " imply the latter declaration" in {
    val declaration1 = "x" :| "X"
    val declaration2 = "y" :| "Y"
    VarRule(Judgement(SimplyTypedDerivationContext(List(declaration1, declaration2)), declaration2)) should be(Some(Judgement(SimplyTypedDerivationContext(List(declaration1, declaration2)), declaration2)))
  }

  "A context containing a judgement" should "imply that judgement regardless of where it is in the context" in {
    val declaration1 = "x" :| "X"
    val declaration2 = "y" :| "Y"
    val declaration3 = "z" :| "Z"
    val declaration4 = "a" :| "A"

    val l = List(declaration1, declaration2, declaration3, declaration4)

    def switch(x: Int, y: Int, list: List[Statement]): List[Statement] = {
      if (x == y) return list
      var lb: ListBuffer[Statement] = ListBuffer[Statement]()
      for (i <- list.indices) {
        i match {
          case `x` => lb += list(y)
          case `y` => lb += list(x)
          case _ => lb += list(i)
        }
      }
      lb.toList
    }

    for (i <- 1 to 3) {
      for (j <- 2 to 3) {
        val ctx1 = SimplyTypedDerivationContext(l)
        val ctx2 = SimplyTypedDerivationContext(switch(i, j, l))
        val jdg1: Option[Judgement[Statement]] = VarRule(Judgement(ctx1, declaration1))
        val jdg2 = Option(Judgement(ctx2, declaration1))
        if (jdg1 != None & jdg2 != None) {
          jdg1.get.subject should be(jdg2.get.subject)
        }
      }
    }

  }

  //Negative cases

  "An empty context" should "not beget any judgements" in {
    VarRule(Judgement(SimplyTypedDerivationContext(), Statement("x", "X"))) should be(None)
  }

  "A context containing only a declaration" should " not imply another declaration" in {
    val oneDeclaration = "x" :| "X"
    val anotherDeclaration = "x" :| "Y"
    VarRule(Judgement(SimplyTypedDerivationContext(List(oneDeclaration)), anotherDeclaration)) should be(None)
  }


}
