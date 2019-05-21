package com.dakers.lambda.lambda2

import com.dakers.lambda._
import com.dakers.lambda.stlc.VarType
import org.scalatest.{FlatSpec, Matchers}

class AbstRuleTest extends FlatSpec with Matchers with STNotation with UTNotation with L2Notation {

  "Abstraction from an empty context" should "throw an exception" in {
    val utTerm: UTTerm = "x"
    val abstVar: Var = Var("x")
    val abstTerm = Abst(utTerm, abstVar)
    AbstRule(L2Judgement(L2Context(), L2Statement(abstTerm, ArrType2("X", "Y"))), "x", VarType("c")) should be(None)
  }

  "Abstraction from a context with a variable declaration" should "create a valid abstraction term" in {
    val utTerm: UTTerm = "x"
    val abstVar: Var = Var("x")
    val abstTerm = Abst(utTerm, abstVar)
    val vt = VarType(abstVar.varName)
    val varDec: L2Statement = "x" :|| "X"
    val expectedStmt = L2Statement(/|("x", "y"), ArrType2("X", "Y"))
    AbstRule(L2Judgement(L2Context(List(varDec), List()), L2Statement(Var("y"), "Y")), "x", VarType("X")) should be(Some(L2Judgement(L2Context(), expectedStmt)))
  }

  "Abstraction from a context with multiple variable declarations" should "create a valid abstraction term" in {
    val utTerm: UTTerm = "x"
    val abstVar: Var = Var("x")
    val abstTerm = Abst(utTerm, abstVar)
    val vt = VarType(abstVar.varName)
    val varDec1: L2Statement = "x" :|| "X"
    val varDec2: L2Statement = "z" :|| "Z"
    val expectedStmt = L2Statement(/|("x", "y"), ArrType2("X", "Y"))
    AbstRule(L2Judgement(L2Context(List(varDec1, varDec2), List()), L2Statement(Var("y"), "Y")), "x", VarType("X")) should be(Some(L2Judgement(L2Context(List(varDec2)), expectedStmt)))
  }

  "The identity function" should "be represented as a polymorphic term dependent on a type" in {
    val origTerm = Var("x")
    val typeName = "X"
    val typeDecl = L2TypeDecl(typeName)
    val ctx = L2Context()
    ctx.add(typeDecl)
    AbstRule2(L2Judgement(ctx, L2Statement(origTerm, VarType("Z"))), typeName) should be(Some(L2Judgement(L2Context(), origTerm :|| π("Z", VarType(typeName)))))
  }


}
