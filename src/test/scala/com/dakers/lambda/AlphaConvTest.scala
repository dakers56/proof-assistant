package com.dakers.lambda

import org.scalatest._


class AlphaConvTest extends FlatSpec with Matchers {

  val binder = AbstOp
  val sep = AbstSep

  //Case: term is a variable

  "The term y " should " occur in the term y." in {
    AlphaConv.isInTerm(Var("y"), "y") should be(true)
  }

  "The term y " should "not occur in the term x." in {
    AlphaConv.isInTerm(Var("x"), "y") should be(false)
  }

  //Case: term is an application

  //Sub-case: application of variables
  "The term y " should " occur in the term yx." in {
    AlphaConv.isInTerm(App(Var("y"), Var("x")), "y") should be(true)
  }


  "The term y " should " not occur in the term xz." in {
    AlphaConv.isInTerm(App(Var("x"), Var("z")), "y") should be(false)
  }


  //Case: term is an abstraction

  //Sub-case: abstraction over variables
  "The term y " should s" occur in the term ($binder)y($sep)y" in {
    AlphaConv.isInTerm(Abst(Var("y"), Var("y")), "y") should be(true)
  }

  "The term y " should s" not occur in the term ($binder)x($sep)x" in {
    AlphaConv.isInTerm(Abst(Var("x"), Var("x")), "y") should be(false)
  }

  //Sub-case: abstraction over application
  "The term y " should s" occur in the term ($binder)x($sep)xz." in {
    AlphaConv.isInTerm(Abst(App(Var("x"), Var("y")), Var("z")), "y") should be(true)
  }

  "The term y " should s" not occur in the term ($binder)x($sep)xz." in {
    AlphaConv.isInTerm(Abst(App(Var("x"), Var("z")), Var("z")), "y") should be(false)
  }

  //Sub-case: abstraction over abstraction
  "The term y " should s" occur in the term ($binder)z($sep)($binder)y($sep)y." in {
    AlphaConv.isInTerm(Abst(Abst(Var("y"), Var("z")), Var("y")), "y") should be(true)
  }

  "The term y " should s" not occur in the term ($binder)z($sep)($binder)y($sep)y." in {
    AlphaConv.isInTerm(Abst(Abst(Var("x"), Var("z")), Var("x")), "y") should be(false)
  }

  //Renaming terms

  //Case: Abstraction terms

  //Sub-case: Abstraction over a variable
  s"The  function ($binder)x($sep)x" should "be alpha convertible to ($binder)y($sep)y" in {
    AlphaConv.Mxy(Abst(Var("x"), Var("x")), "x", "y") should be(Abst(Var("y"), Var("y")))
  }

  s"The  function ($binder)x($sep)x" should "be alpha convertible to ($binder)z($sep)z" in {
    AlphaConv.Mxy(Abst(Var("x"), Var("x")), "x", "z") should be(Abst(Var("z"), Var("z")))
  }

  //Sub-case: Abstraction over an application
  s"The  function ($binder)x($sep)x" should "be renamed to ($binder)y($sep)y" in {
    AlphaConv.Mxy(Abst(App(Var("x"), Var("z")), Var("x")), "x", "y") should be(Abst(App(Var("y"), Var("z")), Var("y")))
  }

  s"The  function ($binder)x($sep)xzu" should s"be renamed to ($binder)y($sep)yzu" in {
    AlphaConv.Mxy(Abst(App(Var("x"), App(Var("z"), Var("u"))), Var("x")), "u", "y") should be(Abst(App(Var("x"), App(Var("z"), Var("y"))), Var("x")))
  }

  s"The  function ($binder)x($sep)xzu" should s"be renamed to ($binder)y($sep)yzu when y replaces x" in {
    AlphaConv.Mxy(Abst(App(Var("x"), App(Var("z"), Var("u"))), Var("x")), "x", "y") should be(Abst(App(Var("y"), App(Var("z"), Var("u"))), Var("y")))
  }

  s"The  function ($binder)x($sep)xzu" should s"be renamed to ($binder)y($sep)yzu when y replaces z" in {
    val zu = App(Var("z"), Var("u"))
    val xzu = App(Var("x"), zu)
    val yu = App(Var("y"), Var("u"))
    val xyu = App(Var("x"), yu)
    AlphaConv.Mxy(Abst(xzu, Var("x")), "z", "y") should be(Abst(xyu, Var("x")))
  }

  s"The  function ($binder)x($sep)xzuw" should s"be renamed to ($binder)y($sep)yzuw when y replaces x" in {
    val zu = App(Var("z"), Var("u"))
    val xzu = App(Var("x"), zu)
    val xzuw = App(xzu, Var("w"))

    val yzu = App(Var("y"), zu)
    val yzuw = App(yzu, Var("w"))

    AlphaConv.Mxy(Abst(xzuw, Var("x")), "x", "y") should be(Abst(yzuw, Var("y")))
  }

  s"($binder)x($sep)xzuw " should s" be renamed to ($binder)x($sep)xyuw when y replaces z" in {
    val xzuw = App(App(Var("x"), App(Var("z"), Var("u"))), Var("w"))
    val _xzuw = Abst(xzuw, Var("x"))

    val xyuw = App(App(Var("x"), App(Var("y"), Var("u"))), Var("w"))
    val _xyuw = Abst(xyuw, Var("x"))

    AlphaConv.Mxy(_xzuw, "z", "y") should be(_xyuw)
  }

  // Associativity of previous test
  s"($binder)x($sep)x(zuw) " should s" be renamed to ($binder)x($sep)x(yuw) when y replaces z " in {
    val x_zuw = App(Var("x"), App(Var("z"), App(Var("u"), Var("w"))))
    val x_yuw = App(Var("x"), App(Var("y"), App(Var("u"), Var("w"))))
    AlphaConv.Mxy(x_zuw, "z", "y") should be(x_yuw)
  }

  s"($binder)xz($sep)xzw " should " be renamed to ($binder)xy($sep)xyw when y replaces z" in {
    println(s"($binder)xz($sep)xzuw " + Abst(Abst(App(App(Var("x"), Var("z")), Var("w")), Var("x")), Var("z")))
    AlphaConv.Mxy(Abst(Abst(App(App(Var("x"), Var("z")), Var("w")), Var("x")), Var("z")), "z", "y") should be(Abst(Abst(App(App(Var("x"), Var("y")), Var("w")), Var("x")), Var("y")))
  }

  s"($binder)xz($sep)xzuw " should " be renamed to ($binder)xy($sep)xyuw when y replaces z" in {
    println(s"($binder)xz($sep)xzuw " + Abst(Abst(App(App(Var("x"), Var("z")), Var("w")), Var("x")), Var("z")))
    AlphaConv.Mxy(Abst(Abst(App(App(App(Var("x"), Var("u")), Var("z")), Var("w")), Var("x")), Var("z")), "z", "y") should be(Abst(Abst(App(App(App(Var("x"), Var("u")), Var("y")), Var("w")), Var("x")), Var("y")))
  }


  /**
   * Beginning of tests for substitution
   */

  //Case: term comprised of a single variable

  "x[x:=N] " should " equal N" in {
    val N = Var("N")
    List("a", "b", "c", "d", "EEEER", "<!@#SDF").foreach(value => AlphaConv.subst(Var(value), N, value) should be(N))
  }

  "y[x:=N] " should " equal y" in {
    val y = Var("y")
    List("a", "b", "c", "d", "EEEER", "<!@#SDF").foreach(value => AlphaConv.subst(y, Var(value), value) should be(y))
  }

  //Case: Application of variables
  "xy[x:=z]" should "equal zy" in {
    AlphaConv.subst(App(Var("x"), Var("y")), Var("z"), "x") should be(App(Var("z"), Var("y")))
  }

//  //Case: Abstraction over a single variable
//  s"($binder)x($sep)x[x:=y]" should " throw an exception" in {
//    intercept[RuntimeException](
//      AlphaConv.subst(Abst(Var("x"), Var("x")), Var("y"), "x")
//    )
//  }

  s"($binder)x($sep)x[y:=x]" should " equal x" in {
    val idFunc = Abst(Var("x"), Var("x"))
    AlphaConv.subst(idFunc, Var("y"), "y") should be(idFunc)
  }

  //Case: application of application terms
  s"xyxz[x:=a]" should " equal ayaz" in {
    val xy = App(Var("x"), Var("y"))
    val xz = App(Var("x"), Var("z"))
    val xyxz = App(xy, xz)

    val ay = App(Var("a"), Var("y"))
    val az = App(Var("a"), Var("z"))
    val ayaz = App(ay, az)

    AlphaConv.subst(xyxz, Var("a"), "x") should be(ayaz)
  }

  s"xyxz[y:=a]" should " equal xaxz" in {
    val xy = App(Var("x"), Var("y"))
    val xz = App(Var("x"), Var("z"))
    val xyxz = App(xy, xz)

    val xa = App(Var("x"), Var("a"))
    val xaxz = App(xa, xz)

    AlphaConv.subst(xyxz, Var("a"), "y") should be(xaxz)
  }

  //Case: application of abstraction terms

  s"($binder)x($sep)xy[y:=z]" should s"equal ($binder)x($sep)xz" in {
    AlphaConv.subst(Abst(App(Var("x"), Var("y")), Var("x")), Var("z"), "y") should be(Abst(App(Var("x"), Var("z")), Var("x")))
  }

  s"($binder)x($sep)xy($binder)z($sep)zy[y:=w]" should s"equal ($binder)x($sep)xw($binder)z($sep)zw" in {
    val xy = Abst(App(Var("x"), Var("y")), Var("x"))
    val zy = Abst(App(Var("z"), Var("y")), Var("z"))
    AlphaConv.subst(App(xy, zy), Var("w"), "y") should be(App(Abst(App(Var("x"), Var("w")), Var("x")), Abst(App(Var("z"), Var("w")), Var("z"))))
  }

}
