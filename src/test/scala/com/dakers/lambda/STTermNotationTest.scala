package com.dakers.lambda

import org.scalatest.{FlatSpec, Matchers}

class STTermNotationTest extends FlatSpec with Matchers with STTermNotation with UTTermNotation {

  "x :| X" should "create a term x of type X" in {
    val stTerm = "x" :| "X"
    stTerm.term should be(Var("x"))
    stTerm.sType should be(VarType("X"))
  }

  "x :| X->Y" should "create a term x of type X->Y" in {
    val stTerm = "x" :| "X".vt -> "Y"
    stTerm.term should be(Var("x"))
    stTerm.sType should be(ArrType("X".vt, "Y".vt))
  }

  "x :| X->Y->Z" should "create a term x of type X->Y->Z" in {
    val stTerm = "x" :| "X".vt -> "Y" -> "Z"
    stTerm.term should be(Var("x"))
    stTerm.sType should be(ArrType(ArrType("X".vt, "Y".vt), "Z".vt))
  }

}
