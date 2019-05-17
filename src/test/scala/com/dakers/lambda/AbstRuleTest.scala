package com.dakers.lambda

import com.dakers.lambda.VarRule.AbstRule
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable.ListBuffer

class AbstRuleTest extends FlatSpec with Matchers with STNotation with UTNotation {

  //Positive cases
  "x : T|- x : T " should "imply that |- /|x.x : T -> T" in {
    val ctx = SimplyTypedDerivationContext(List("x" :| "T".tv))
    val j1 = Judgement(ctx, "x" :| "T")
    AbstRule(j1) should be(Some(Judgement(SimplyTypedDerivationContext(), /|("x", "x") :| "T" ->: "T".tv)))
  }

  //Abstracting over the same variable
  "y : U, x : T|- x : T " should "imply that |- /|x.x : T -> T" in {
    val list: ListBuffer[Statement] = ListBuffer[Statement]()
    list += "y" :| "U"
    list += "x" :| "T"
    val ctx = SimplyTypedDerivationContext(list.toList)
    val j1 = Judgement(ctx, "x" :| "T")
    AbstRule(j1) should be(Some(Judgement(SimplyTypedDerivationContext(List("y" :| "U")), /|("x", "x") :| "T" ->: "T".tv)))
  }

  //Abstracting over a different variable of the same type
  "x : T, y : T|- x : T " should "imply that |- /|y.x : T -> T" in {
    val list: ListBuffer[Statement] = ListBuffer[Statement]()
    list += "x" :| "T"
    list += "y" :| "T"
    val ctx = SimplyTypedDerivationContext(list.toList)
    val j1 = Judgement(ctx, "x" :| "T")
    AbstRule(j1) should be(Some(Judgement(SimplyTypedDerivationContext(List("x" :| "T")), /|("y", "x") :| "T" ->: "T".tv)))
  }

  //Abstracting over a different variable of a different type
  "x : T, y : U|- x : T " should "imply that |- /|y.x : U -> T" in {
    val list: ListBuffer[Statement] = ListBuffer[Statement]()
    list += "x" :| "T"
    list += "y" :| "U"
    val ctx = SimplyTypedDerivationContext(list.toList)
    val j1 = Judgement(ctx, "x" :| "T")
    AbstRule(j1) should be(Some(Judgement(SimplyTypedDerivationContext(List("x" :| "T")), /|("y", "x") :| "U" ->: "T".tv)))
  }

  "Abstracting over an application: x : T|- xy : T " should "imply that |- /|x.xy : T -> T" in {
    val ctx = SimplyTypedDerivationContext(List("x" :| "T".tv))
    val j1 = Judgement(ctx, "x" * "y" :| "T")
    AbstRule(j1) should be(Some(Judgement(SimplyTypedDerivationContext(), /|("x", "x" * "y") :| "T" ->: "T".tv)))
  }

  //Abstracting over the same variable
  "Abstracting over an application: y : U, x : T|- xz : T " should "imply that |- /|x.xz : T -> T" in {
    val list: ListBuffer[Statement] = ListBuffer[Statement]()
    list += "y" :| "U"
    list += "x" :| "T"
    val ctx = SimplyTypedDerivationContext(list.toList)
    val j1 = Judgement(ctx, "x" * "z" :| "T")
    AbstRule(j1) should be(Some(Judgement(SimplyTypedDerivationContext(List("y" :| "U")), /|("x", "x" * "z") :| "T" ->: "T".tv)))
  }

  //Abstracting over a different variable of the same type
  "Abstracting over an application: x : T, y : T|- xz : T " should "imply that |- /|y.xz : T -> T" in {
    val list: ListBuffer[Statement] = ListBuffer[Statement]()
    list += "x" :| "T"
    list += "y" :| "T"
    val ctx = SimplyTypedDerivationContext(list.toList)
    val j1 = Judgement(ctx, "x" * "z" :| "T")
    AbstRule(j1) should be(Some(Judgement(SimplyTypedDerivationContext(List("x" :| "T")), /|("y", "x" * "z") :| "T" ->: "T".tv)))
  }

  //Abstracting over a different variable of a different type
  "Abstracting over an application: x : T, y : U|- x : T " should "imply that |- /|y.x : U -> T" in {
    val list: ListBuffer[Statement] = ListBuffer[Statement]()
    list += "x" :| "T"
    list += "y" :| "U"
    val ctx = SimplyTypedDerivationContext(list.toList)
    val j1 = Judgement(ctx, "x" * "y" :| "T")
    AbstRule(j1) should be(Some(Judgement(SimplyTypedDerivationContext(List("x" :| "T")), /|("y", "x" * "y") :| "U" ->: "T".tv)))
  }

  //Abstracting over an abstraction with a variable of the same type
  "Abstracting over an abstraction: x : T, z : T|- /|x.x : T -> T " should "imply that |- /|z./|x.x : T -> T -> T" in {
    val ctx = SimplyTypedDerivationContext(List("x" :| "T".tv, "z" :| "T".tv))
    val j1 = Judgement(ctx, /|("x", "x") :| "T" ->: "T".tv)
    AbstRule(j1) should be(Some(Judgement(SimplyTypedDerivationContext(List("x" :| "T".tv)), /|("z", /|("x", "x")) :| "T".tv ->: "T" ->: "T".tv)))
  }

  //Abstracting over an abstraction with a variable of the same type
  "Abstracting over an abstraction: x : T, z : U|- /|x.x : T -> T " should "imply that |- /|z./|x.x : U -> T -> T" in {
    val ctx = SimplyTypedDerivationContext(List("x" :| "T".tv, "z" :| "U".tv))
    val j1 = Judgement(ctx, /|("x", "x") :| "T" ->: "T".tv)
    AbstRule(j1) should be(Some(Judgement(SimplyTypedDerivationContext(List("x" :| "T".tv)), /|("z", /|("x", "x")) :| "U".tv ->: "T" ->: "T".tv)))
  }

  //Negative cases

  "Abstraction over a judgement with an empty context" should "return None" in {
    val ctx = SimplyTypedDerivationContext(List())
    AbstRule(Judgement(ctx, /|("x", "x") :| "T".tv ->: "T".tv)) should be(None)
  }

  "Abstraction over a judgement with a context not ending in a declaration" should "return None" in {
    var ctx = SimplyTypedDerivationContext(List(/|("z", "z") :| "Z"))
    AbstRule(Judgement(ctx, /|("x", "x") :| "T".tv ->: "T".tv)) should be(None)

    ctx = SimplyTypedDerivationContext(List("z" * "z" :| "Z"))
    AbstRule(Judgement(ctx, /|("x", "x") :| "T".tv ->: "T".tv)) should be(None)
  }
}
