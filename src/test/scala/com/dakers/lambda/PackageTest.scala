package com.dakers.lambda

import org.scalatest.{FlatSpec, Matchers}

class PackageTest extends FlatSpec with Matchers {

    "x" should "deserialize to Var(x)" in {
      lam("x") should be(Var("x"))
    }

    s"$AbstOp x$AbstSep x" should "deserialize to Abst(Var(x),Var(x))" in {
      lam(s"/|x.x") should be(Abst(Var("x"), Var("x")))
    }

    s"$AbstOp x$AbstSep xy" should "deserialize to Abst(App(Var(x), Var(y)),Var(x))" in {
      lam(s"/|x.x") should be(Abst(Var("x"), Var("x")))
    }

    s"$AbstOp x $AbstSep $AbstOp y $AbstSep xy" should "deserialize to Abst(App(Var(x), Var(y)),Var(x))" in {
      lam(s"/|x./|y.(xy)") should be(Abst(Abst(App(Var("x"), Var("y")), Var("y")), Var("x")))
    }

    s"$AbstOp z $AbstSep $AbstOp x $AbstSep $AbstOp y $AbstSep xyz" should "deserialize to Abst(App(Var(x), Var(y)),Var(x))" in {
      lam(s"/|z./|x./|y.((xy)z)") should be(Abst(Abst(Abst(App(App(Var("x"), Var("y")), Var("z")), Var("y")), Var("x")), Var("z")))
    }

    s"$AbstOp u $AbstSep $AbstOp z $AbstSep $AbstOp x $AbstSep $AbstOp y $AbstSep xyz" should "deserialize to Abst(Abst(Abst(Abst(App(App(App(Var(\"x\"), Var(\"y\")), Var(\"z\")), Var(\"u\")), Var(\"y\")), Var(\"x\")), Var(\"z\")), Var(\"u\"))" in {
      lam(s"/|u./|z./|x./|y.(((xy)z)u)") should be(Abst(Abst(Abst(Abst(App(App(App(Var("x"), Var("y")), Var("z")), Var("u")), Var("y")), Var("x")), Var("z")), Var("u")))
    }

    s"$AbstOp w $AbstOp u $AbstSep $AbstOp z $AbstSep $AbstOp x $AbstSep $AbstOp y $AbstSep xyzu $AbstOp t $AbstSep t" should "deserialize to Abst(Abst(Abst(Abst(App(App(App(Var(\"x\"), Var(\"y\")), Var(\"z\")), Var(\"u\")), Var(\"y\")), Var(\"x\")), Var(\"z\")), Var(\"u\"))" in {
      lam(s"/|w./|u./|z./|x./|y.((((xy)z)u)w)") should be(
        Abst(Abst(Abst(Abst(Abst(App(App(App(App(Var("x"), Var("y")), Var("z")), Var("u")), Var("w")), Var("y")), Var("x")), Var("z")), Var("u")), Var("w")))
    }

    s"$AbstOp w $AbstOp u $AbstSep $AbstOp z $AbstSep $AbstOp x $AbstSep $AbstOp y $AbstSep xyzu" should "deserialize to Abst(Abst(Abst(Abst(App(App(App(Var(\"x\"), Var(\"y\")), Var(\"z\")), Var(\"u\")), Var(\"y\")), Var(\"x\")), Var(\"z\")), Var(\"u\"))" in {
      val xyzuw = App(App(App(App(Var("x"), Var("y")),Var("z")),Var("u")),Var("w"))
      val xyzuw_t = App(xyzuw,Abst(Var("t"), Var("t")))

      lam(s"/|w./|u./|z./|x./|y.(((((xy)z)u)w)/|t.t)") should be(Abst(Abst(Abst(Abst(Abst(xyzuw_t,Var("y")),Var("x")),Var("z")),Var("u")),Var("w")))
    }

    s"$AbstOp x $AbstSep $AbstOp y $AbstSep x($AbstOp z $AbstSep y)" should "deserialize to Abst(App(Var(x), Var(y)),Var(x))" in {
      val z_y = Abst(Var("y"), Var("z"))
      val xz_y = App(Var("x"), z_y)
      lam(s"/|x./|y.(x/|z.y)") should be(Abst(Abst(xz_y,Var("y")),Var("x")))
    }

    s"$AbstOp x $AbstSep $AbstOp y $AbstSep  ( x($AbstOp z $AbstSep y) $AbstOp a $AbstSep a)" should "deserialize to Abst(App(Var(x), Var(y)),Var(x))" in {
      val z_y = Abst(Var("y"), Var("z"))
      val xz_y = App(Var("x"), z_y)
      lam(s"/|x./|y.((x/|z.y)/|a.a)") should be(Abst(Abst(App(xz_y, Abst(Var("a"), Var("a"))),Var("y")),Var("x")))
    }

  s"$AbstOp x $AbstSep $AbstOp y $AbstSep  ( x($AbstOp z $AbstSep ()) $AbstOp a $AbstSep a)" should "deserialize to Abst(App(Var(x), Var(y)),Var(x))" in {
    lam(s"(/|b.b/|a.a)") should be(App(Abst(Var("b"), Var("b")), Abst(Var("a"), Var("a"))))
  }

    s"An abstraction of multiple variables over an application including an application of an abstraction to another abstraction" should "deserialize properly" in {
      val aa_bb = App(Abst(Var("b"), Var("b")), Abst(Var("a"), Var("a")))
      lam(s"/|x./|y.((x/|z.y)(/|b.b/|a.a))") should be(Abst(Abst(App(App(Var("x"), Abst(Var("y"), Var("z"))), aa_bb), Var("y")), Var("x")))
    }

  s"An abstraction of multiple variables over an application including an application of an abstraction to another abstraction on the left" should "deserialize properly" in {
    val aa_bb = App(Abst(Var("b"), Var("b")), Abst(Var("a"), Var("a")))
    lam(s"/|x./|y.((/|b.b/|a.a)(x/|z.y))") should be(Abst(Abst(App(aa_bb, App(Var("x"), Abst(Var("y"), Var("z")))), Var("y")), Var("x")))
  }


}
