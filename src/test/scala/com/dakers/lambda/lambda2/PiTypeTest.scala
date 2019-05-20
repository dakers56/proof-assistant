package com.dakers.lambda.lambda2

import com.dakers.lambda.lambda2.π.subst
import com.dakers.lambda.stlc.{ArrType, VarType}
import com.dakers.lambda.{STNotation, UTNotation}
import org.scalatest.{FlatSpec, Matchers};

class PiTypeTest extends FlatSpec with Matchers with L2Notation with UTNotation with STNotation {

  "Beta reducing a pi term containing only a single term" should "reduce to a VarType" in {
    val orig = π("T", "T".tv)
    val toBind: VarType = "T".tv

    for (i <- 'a' to 'z') {
      val toSub: VarType = i.toString.tv
      val after = subst(orig, toBind, toSub)
      after should be(toSub)
      after.free() should be(Set(toSub))
      after.bound() should be(empty)

    }
  }

  "Beta reducing a pi term containing only a single term bound variable and a free variable" should "reduce to an ArrType2 with no bound variables" in {
    val orig = π(ArrType("T", "U"), "T".tv)
    val toBind: VarType = "T".tv

    for (i <- 'a' to 'z') {
      val toSub: VarType = i.toString.tv
      val after = subst(orig, toBind, toSub)

      after should be(ArrType2(toSub, "U"))
      after.free() should be(Set(i.toString().tv, "U".tv))
      after.bound() should be(empty)

    }
  }

  "Beta reducing a pi term containing another pi term (with a different bound variable)" should "reduce to a pi term with one fewer bound variable" in {
    val orig = π(π(ArrType2("T", "U"), "U".tv), "T".tv)
    orig.free() should be(empty)
    orig.bound() should be(Set("T".tv, "U".tv))
    val toBind: VarType = "T".tv

    for (i <- 'a' to 'z') {
      val toSub: VarType = i.toString.tv
      val after = subst(orig, toBind, toSub)
      after should be(π(ArrType2(toSub, "U"), "U".tv))
      after.free() should be(Set(i.toString().tv))
      after.bound() should be(Set("U".tv))

    }
  }

  "Beta reducing a pi term containing another pi term (with a different bound variable) containing  third pi term" should "reduce to a pi term with one fewer bound variable" in {
    val orig = π(ArrType2("T".tv, π(ArrType2("U".tv, π("V", "V".tv)), "U".tv)), "T".tv)
    orig.free() should be(empty)
    orig.bound() should be(Set("V".tv, "T".tv, "U".tv))
    val toBind: VarType = "T".tv

    for (i <- 'a' to 'z') {
      val toSub: VarType = i.toString.tv
      val after = subst(orig, toBind, toSub)

      after should be(ArrType2(toSub, π(ArrType2("U".tv, π("V", "V".tv)), "U".tv)))
      after.free() should be(Set(i.toString().tv))
      after.bound() should be(Set("U".tv, "V".tv))

    }
  }


  "Beta reducing a type which is not the binding type" should "yield the original type" in {
    subst("X".tv, "Y".tv, "Z".tv) should be("X".tv)
    subst(ArrType("X".tv, "A".tv), "Y".tv, "Z".tv) should be(ArrType2("X".tv, "A".tv))
    subst(ArrType2("X".tv, "A".tv), "Y".tv, "Z".tv) should be(ArrType2("X".tv, "A".tv))
  }
}
