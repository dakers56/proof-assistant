package com.dakers.lambda

import org.scalatest.{FlatSpec, Matchers}

class STTermNotationTest extends FlatSpec with Matchers with STTermNotation with UTTermNotation {

  "x :| X" should "create a term x of type X" in {
    val stTerm = "x" :| "X"
    stTerm.term should be(Var("x"))
    stTerm.sType should be(VarType("X"))
  }

  "x :| X->Y" should "create a term x of type X->Y" in {
    val stTerm = "x" :| "X".vt ->: "Y".vt
    stTerm.term should be(Var("x"))
    stTerm.sType should be(ArrType("X".vt, "Y".vt))
  }

  "x :| X->Y->Z" should "create a term x of type X->Y->Z" in {
    val stTerm = "x" :| "X".vt ->: "Y".vt ->: "Z".vt
    stTerm.term should be(Var("x"))
    stTerm.sType should be(ArrType("X".vt, ArrType("Y".vt, "Z".vt)))
  }

  "x :| X->Y->Z->A" should "create a term x of type X->Y->Z->A" in {
    val stTerm = "x" :| "X".vt ->: "Y".vt ->: "Z".vt ->: "A".vt
    stTerm.term should be(Var("x"))
    stTerm.sType should be(ArrType("X", ArrType("Y", ArrType("Z", "A"))))
  }

  "x :| X->Y->(Z->A)" should "create a term x of type X->Y->(Z->A)" in {
    val stTerm = "x" :| "X".vt ->: "Y".vt ->: ("Z".vt ->: "A".vt)
    stTerm.term should be(Var("x"))
    stTerm.sType should be(ArrType("X", ArrType("Y", ArrType("Z", "A"))))
  }

  "x :| X->(Y->Z)->A" should "create a term x of type X->Y->(Z->A)" in {
    val stTerm = "x" :| "X".vt ->: ("Y".vt ->: "Z".vt) ->: "A".vt
    stTerm.term should be(Var("x"))
    stTerm.sType should be(ArrType("X".vt, ArrType(ArrType("Y".vt, "Z".vt), "A".vt)))
  }

}
