package com.dakers.lambda

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.Range

class BetaRedTest extends FlatSpec with Matchers with UTTermNotation {
  val binder = AbstOp
  val sep = AbstSep

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

  s"y($binder)x($sep)x" should " have exactly no redexes" in {
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

  /*Testing using lambda package "dialect"*/

  "/|x. (xy) " should " have no redex" in {
    val appTerm = "x" * "y"
    BetaRed.redexes(/|("x", appTerm)) should be(List())
  }

  "(/|x. x)y " should " have exactly one redex" in {
    val abstTerm = /|("x", "x")
    val appTerm = abstTerm * "y"
    BetaRed.redexes(appTerm) should be(List(App(Abst(Var("x"), Var("x")), Var("y"))))
  }


  "(/|x.(/|y.yx)z)v " should " have exactly two redexes" in {
    val y_yx = /|("y", "y" * "x")
    val x_y_yx_z = /|("x", y_yx * "z")
    val x_y_yx_z_v = x_y_yx_z * "v"
    BetaRed.redexes(x_y_yx_z * "v") should be(List(x_y_yx_z_v, y_yx * "z"))
  }

  "(/|z.u(/|x.(/|y.yx)z))v " should " have exactly three redexes" in {
    val y_yx = /|("y", "y" * "x")
    val x_y_yx_z = /|("x", y_yx * "z")
    val x_y_yx_z_v = x_y_yx_z * "v"
    val fullTerm = /|("z", x_y_yx_z_v) * "u"
    BetaRed.redexes(fullTerm) should be(List(fullTerm, x_y_yx_z_v, y_yx * "z"))
  }

  "Applying an abstraction term to another term " should " add exactly one redex" in {
    var t = /|("x", "x") * "y"
    var expected = List[App](t)
    for (i <- Range(1, 10)) {
      val xn = "x" + i.toString
      t = /|(xn, t) * i.toString
      expected = t :: expected
      BetaRed.redexes(t) should be(expected)
    }
  }

  "Abstracting over a term should " should " add no redexes" in {
    var t: UTTerm = /|("x", "x") * "y"
    val expected = List[App](t.asInstanceOf[App])
    for (i <- Range(1, 10)) {
      val xn = "x" + i.toString
      t = /|(xn, t)
      BetaRed.redexes(t) should be(expected)
    }
  }

  "A term with a redex on the right and none on the left" should " have exactly one redex" in {
    val redexR = /|("y", "x") * "z"
    val appL = "a" * "b" * "c" * "d"
    BetaRed.redexes(redexR) should be(List(redexR))
  }

  "A term with a redex in the middle " should " have as many redexes as there are redex terms to its right" in {
    val lApp = "a" * "b" * "c" * "d"
    val midRedex = /|("y", "y") * "z"
    var rApp = "u"
    var fullTerm = lApp * midRedex * rApp
    var expected = List(midRedex)
    BetaRed.redexes(fullTerm) should be(expected)
    for (i <- Range(1, 100)) {
      val newVar = ("x" + i.toString)
      val newTerm = /|(newVar, newVar) * (i + "x")
      expected = newTerm :: expected
      fullTerm = fullTerm * newTerm
      BetaRed.redexes(fullTerm) should contain theSameElementsAs (expected)
    }
  }
}
