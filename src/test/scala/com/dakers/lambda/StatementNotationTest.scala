package com.dakers.lambda

import org.scalatest.{FlatSpec, Matchers}

class StatementNotationTest extends FlatSpec with Matchers with STNotation with UTNotation {

  // Test a term
  "x :| X" should "create a term x of type X" in {
    val stTerm = "x" :| "X"
    stTerm.term should be(Var("x"))
    stTerm.sType should be(VarType("X"))
  }

  // Test the most basic arrow type
  "x :| X->Y" should "create a term x of type X->Y" in {
    val stTerm = "x" :| "X".tv ->: "Y".tv
    stTerm.term should be(Var("x"))
    stTerm.sType should be(ArrType("X".tv, "Y".tv))
  }

  // Test the next simplest arrow type
  "x :| X->Y->Z" should "create a term x of type X->Y->Z" in {
    val stTerm = "x" :| "X".tv ->: "Y".tv ->: "Z".tv
    stTerm.term should be(Var("x"))
    stTerm.sType should be(ArrType("X".tv, ArrType("Y".tv, "Z".tv)))
  }

  // Test the next simplest arrow type
  "x :| X->Y->Z->A" should "create a term x of type X->Y->Z->A" in {
    val stTerm = "x" :| "X".tv ->: "Y".tv ->: "Z".tv ->: "A".tv
    stTerm.term should be(Var("x"))
    stTerm.sType should be(ArrType("X", ArrType("Y", ArrType("Z", "A"))))
  }

  // Test right associativity
  "X -> Y -> Z" should "associate from the right, i.e, be of type X -> (Y -> Z)" in {
    val stTerm = /|("x", /|("y", "z")) :| "X".tv ->: "Y".tv ->: "Z".tv
    stTerm.sType should be(ArrType("X", ArrType("Y", "Z")))

  }

  "x :| X->Y->Z->A" should "associate from the right, i.e, be of type X->(Y->(Z->A))" in {
    val stTerm = "x" :| "X".tv ->: "Y".tv ->: ("Z".tv ->: "A".tv)
    stTerm.term should be(Var("x"))
    stTerm.sType should be(ArrType("X", ArrType("Y", ArrType("Z", "A"))))
  }

  //Test that parentheses can "force" evaluation from the left
  "x :| X->(Y->Z)->A" should "respect parenthesization " in {
    val stTerm = "x" :| "X".tv ->: ("Y".tv ->: "Z".tv) ->: "A".tv
    stTerm.term should be(Var("x"))
    stTerm.sType should be(ArrType("X".tv, ArrType(ArrType("Y".tv, "Z".tv), "A".tv)))
  }

  "x :| X->((Y->Z) -> B)->A" should "respect parenthesization " in {
    val stTerm = "x" :| "X".tv ->: (("Y".tv ->: "Z".tv) ->: "B".tv) ->: "A".tv
    stTerm.term should be(Var("x"))
    stTerm.sType should be(ArrType("X".tv, ArrType(ArrType(ArrType("Y", "Z"), "B"), "A".tv)))
  }

  "x :| (X->Y)->Z" should "respect parenthesization " in {
    val stTerm = "x" :| (("X".tv ->: "Y".tv) ->: "Z".tv)
    stTerm.term should be(Var("x"))
    stTerm.sType should be(ArrType(ArrType("X", "Y"), "Z"))
  }

  "x :| (((X->Y)->Z)->A)" should "respect parenthesization " in {
    val stTerm = "x" :| ((("X".tv ->: "Y".tv) ->: "Z".tv) ->: "A".tv)
    stTerm.term should be(Var("x"))
    stTerm.sType should be(ArrType(ArrType(ArrType("X", "Y"), "Z"), "A"))
  }

}
