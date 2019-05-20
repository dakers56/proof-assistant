package com.dakers.lambda.lambda2

import com.dakers.lambda.stlc.{ArrType, VarType}
import com.dakers.lambda.{STNotation, UTNotation}
import org.scalatest.{FlatSpec, Matchers}

class FBVarsTest extends FlatSpec with Matchers with STNotation with UTNotation with L2Notation {

  "A variable type" should "have exactly one free variable and no bound variables" in {
    VarType("x").free() should be(Set(VarType("x")))
    VarType("x").bound() should be(empty)
  }

  "An arrow type " should "have the union of its constituents' free variables as its own free variables" in {
    val t1: VarType = "x".tv
    val t2: VarType = "y".tv
    ArrType(t1, t2).free should be(Set(t1, t2))
    ArrType(t1, t2).bound should be(empty)
  }

  "An arrow type with terms in common" should "have the union of its constituents' free variables as its own free variables" in {
    val t1: VarType = "x".tv
    ArrType(t1, t1).free should be(Set(t1))
    ArrType(t1, t1).bound should be(empty)
  }

  "A π type over a single variable, where the term to bind is the same as the original term" should "have no free variables and one bound variable" in {
    val v = VarType("v")
    π(v, v).free() should be(empty)
    π(v, v).bound() should be(Set(v))
  }

  "A π type over a single variable, where the term to bind is different from the original term" should "have one free variable and one bound variable" in {
    val v = VarType("v")
    val w = VarType("w")
    π(v, w).free() should be(Set(v))
    π(v, w).bound() should be(Set(w))
  }

  "π(T:*.T->U)" should "have one free variables and one bound variable" in {
    val pi = π(ArrType("T", "U"), VarType("T"))
    pi.free() should be(Set("U".tv))
    pi.bound() should be(Set("T".tv))
  }
  "π(T:*π(U:*.T->U)" should "have no free variables and two bound variables" in {
    val pi = π(π(ArrType("T", "U"), "U".tv), "T".tv)
    pi.free() should be(empty)
    pi.bound() should be(Set("T".tv, "U".tv))
  }

  "π(T:*π(U:*.T->U->V)" should "have one free variable and two bound variable" in {
    val pi = π(π(ArrType("T", ArrType("U", "V")), "U".tv), "T".tv)
    pi.free() should be(Set(VarType("V")))
    pi.bound() should be(Set("T".tv, "U".tv))
  }

  "π(T:*π(U:*.π(T->U->V))" should "have one free variable and two bound variable" in {
    val pi = π(π(ArrType("T", ArrType("U", "V")), "U".tv), "T".tv)
    pi.free() should be(Set(VarType("V")))
    pi.bound() should be(Set("T".tv, "U".tv))
  }

  "ArrType2: π(T:*.T->U)" should "have one free variables and one bound variable" in {
    val pi = π(ArrType("T", "U"), VarType("T"))
    pi.free() should be(Set("U".tv))
    pi.bound() should be(Set("T".tv))

    val pi2 = π(ArrType2("T", "U"), VarType("T"))
    pi2.free() should be(pi.free())
    pi2.bound() should be(pi.bound())
  }

  "ArrType2: π(T:*π(U:*.T->U)" should "behave the same as ArrType" in {
    val pi = π(ArrType("T", "U"), VarType("T"))
    pi.free() should be(Set("U".tv))
    pi.bound() should be(Set("T".tv))

    val pi2 = π(ArrType2("T", "U"), VarType("T"))
    pi2.free() should be(pi.free())
    pi2.bound() should be(pi.bound())
  }

  "ArrType2: π(T:*π(U:*.T->U->V)" should "have one free variable and two bound variable" in {
    val pi = π(π(ArrType("T", ArrType("U", "V")), "U".tv), "T".tv)
    pi.free() should be(Set(VarType("V")))
    pi.bound() should be(Set("T".tv, "U".tv))

    val pi2 = π(π(ArrType2("T", ArrType("U", "V")), "U".tv), "T".tv)
    pi2.free() should be(pi.free())
    pi2.bound() should be(pi.bound())
  }
}
