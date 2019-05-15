package com.dakers.lambda

import org.scalatest.{FlatSpec, Matchers}

class STTermNotationTest extends FlatSpec with Matchers with STTermNotation with UTTermNotation {

  // Test a term
  "x :| X" should "create a term x of type X" in {
    val stTerm = "x" :| "X"
    stTerm.term should be(Var("x"))
    stTerm.sType should be(VarType("X"))
  }

  // Test the most basic arrow type
  "x :| X->Y" should "create a term x of type X->Y" in {
    val stTerm = "x" :| "X".vt ->: "Y".vt
    stTerm.term should be(Var("x"))
    stTerm.sType should be(ArrType("X".vt, "Y".vt))
  }

  // Test the next simplest arrow type
  "x :| X->Y->Z" should "create a term x of type X->Y->Z" in {
    val stTerm = "x" :| "X".vt ->: "Y".vt ->: "Z".vt
    stTerm.term should be(Var("x"))
    stTerm.sType should be(ArrType("X".vt, ArrType("Y".vt, "Z".vt)))
  }

  // Test the next simplest arrow type
  "x :| X->Y->Z->A" should "create a term x of type X->Y->Z->A" in {
    val stTerm = "x" :| "X".vt ->: "Y".vt ->: "Z".vt ->: "A".vt
    stTerm.term should be(Var("x"))
    stTerm.sType should be(ArrType("X", ArrType("Y", ArrType("Z", "A"))))
  }

  // Test right associativity
  "X -> Y -> Z" should "associate from the right, i.e, be of type X -> (Y -> Z)" in {
    val stTerm = /|("x", /|("y", "z")) :| "X".vt ->: "Y".vt ->: "Z".vt
    stTerm.sType should be(ArrType("X", ArrType("Y", "Z")))

  }

  "x :| X->Y->Z->A" should "associate from the right, i.e, be of type X->(Y->(Z->A))" in {
    val stTerm = "x" :| "X".vt ->: "Y".vt ->: ("Z".vt ->: "A".vt)
    stTerm.term should be(Var("x"))
    stTerm.sType should be(ArrType("X", ArrType("Y", ArrType("Z", "A"))))
  }

  //Test that parentheses can "force" evaluation from the left
  "x :| X->(Y->Z)->A" should "respect parenthesization " in {
    val stTerm = "x" :| "X".vt ->: ("Y".vt ->: "Z".vt) ->: "A".vt
    stTerm.term should be(Var("x"))
    stTerm.sType should be(ArrType("X".vt, ArrType(ArrType("Y".vt, "Z".vt), "A".vt)))
  }

  "x :| X->((Y->Z) -> B)->A" should "respect parenthesization " in {
    val stTerm = "x" :| "X".vt ->: (("Y".vt ->: "Z".vt) ->: "B".vt) ->: "A".vt
    stTerm.term should be(Var("x"))
    stTerm.sType should be(ArrType("X".vt, ArrType(ArrType(ArrType("Y", "Z"), "B"), "A".vt)))
  }

  "x :| (X->Y)->Z" should "respect parenthesization " in {
    val stTerm = "x" :| (("X".vt ->: "Y".vt) ->: "Z".vt)
    stTerm.term should be(Var("x"))
    stTerm.sType should be(ArrType(ArrType("X", "Y"), "Z"))
  }

  "x :| (((X->Y)->Z)->A)" should "respect parenthesization " in {
    val stTerm = "x" :| ((("X".vt ->: "Y".vt) ->: "Z".vt) ->: "A".vt)
    stTerm.term should be(Var("x"))
    stTerm.sType should be(ArrType(ArrType(ArrType("X", "Y"), "Z"), "A"))
  }

}
