package com.dakers.lambda

import org.scalatest.{FlatSpec, Matchers}

  class BetaRedTest extends FlatSpec with Matchers {
  val binder = Term.AbstOp
  val sep = Term.AbstSep

  //Case: Variable applied in redex
  s"$binder x $sep xy" should " 1-step beta reduce to y" in {
    BetaRed.red1s(App(Abst(Var("x"), Var("x")), Var("y")), "x") should be(Var("y"))
  }

  s"$binder x $sep xyz" should " 1-step beta reduce to y" in {
    BetaRed.red1s(App(Abst(App(Var("x"), Var("y")), Var("x")), Var("z")), "x") should be(App(Var("z"), Var("y")))
  }


  //Case: Application applied in redex.
  s"($binder x $sep x)yz" should " 1-step beta reduce to y" in {
    BetaRed.red1s(App(Abst(Var("x"), Var("x")), App(Var("y"), Var("z"))), "x") should be(App(Var("y"), Var("z")))
  }

  s"($binder x $sep x)yzu" should " 1-step beta reduce to y" in {
    BetaRed.red1s(App(Abst(Var("x"), Var("x")), App(App(Var("y"), Var("z")), Var("u"))), "x") should be(App(App(Var("y"), Var("z")), Var("u")))
  }

  s"($binder x $sep x)($binder x $sep x)" should "throw an exception" in {
    intercept[RuntimeException](
      BetaRed.red1s(App(Abst(Var("x"), Var("x")), Abst(Var("x"), Var("x"))), "x")
    )
  }

  s"($binder x $sep x)($binder x $sep xy)" should "throw an exception" in {
    intercept[RuntimeException](
      BetaRed.red1s(App(Abst(Var("x"), Var("x")), Abst(App(Var("x"), Var("y")), Var("x"))), "x")
    )
  }

  //Sub-case: Abstracting over an application
  s"($binder x $sep xy)zu" should " 1-step beta reduce to y" in {
    BetaRed.red1s(App(Abst(App(Var("x"), Var("y")), Var("x")), App(Var("z"), Var("u"))), "x") should be(App(App(Var("z"), Var("u")), Var("y")))
  }

  s"($binder x $sep xy)($binder z $sep zu)" should " 1-step beta reduce to ($binder z $sep zu)y" in {
    BetaRed.red1s(App(Abst(App(Var("x"), Var("y")), Var("x")), Abst(App(Var("z"), Var("u")), Var("z"))), "x") should be(App(Abst(App(Var("z"), Var("u")), Var("z")), Var("y")))
  }


  /**
   * Testing redex recognition
   */

  s"($binder)x($sep)x" should " no redexes" in {
    val redex = Abst(Var("x"), Var("x"))
    BetaRed.redexes(redex, List()) should be(List())
  }

  s"x" should " have no redexes" in {
    BetaRed.redexes(Var("x"), List()) should be(List())
  }

  s"y($binder)x($sep)x" should " have exactlyno redexes" in {
    val redex = Abst(Var("x"), Var("x"))
    BetaRed.redexes(App(Var("y"), redex)
      , List()) should be(List())
  }

  s"($binder)y($sep)y($binder)x($sep)x" should " have exactly one redex: ($binder)x($sep)x" in {
    val redex1 = Abst(Var("x"), Var("x"))
    val redex2 = Abst(Var("x"), Var("y"))
    BetaRed.redexes(App(redex1, redex2)
      , List()) should be(List(App(redex1, redex2)))
  }

  s"(($binder)x($sep)(($binder)y($sep)yx)z)v" should " have two redexes: (($binder)x($sep)(($binder)y($sep)yx)z)v and " in {
    val y_yx = Abst(App(Var("y"), Var("x")), Var("y"))
    val y_yx_z = App(y_yx, Var("z"))
    val y_yx_z_v = App(y_yx_z, Var("z"))
    BetaRed.redexes(App(Abst(y_yx_z_v, Var("x")), Var("v"))
      , List()) should be(List(App(Abst(y_yx_z_v, Var("x")), Var("v")), y_yx_z))
  }

}
