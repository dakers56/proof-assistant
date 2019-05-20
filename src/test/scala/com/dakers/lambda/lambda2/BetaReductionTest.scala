package com.dakers.lambda.lambda2

import com.dakers.lambda.lambda2.L2Type.red1s
import com.dakers.lambda.stlc.VarType
import com.dakers.lambda.{L2Conv, STNotation, UTNotation}
import org.scalatest.{FlatSpec, Matchers}

class BetaReductionTest extends FlatSpec with Matchers with UTNotation with STNotation with L2Conv {
  "The type A[A:=B]" should " beta reduce to B" in {
    red1s(VarType("A"), VarType("A"), VarType("B")) should be(VarType("B"))
  }

  "The type A -> B[A:=c]" should " beta reduce to C -> B" in {
    red1s(ArrType2(VarType("A"), VarType("B")), VarType("A"), VarType("C")) should be(ArrType2("C", "B"))
  }

  "The type A -> (B -> C)[A:=D]" should " beta reduce to D -> (B -> C)" in {
    red1s("A".tv ->: ("B" ->: "C".tv)
      , VarType("A")
      , VarType("D")
    ) should be(ArrType2("D", ArrType2("B", "C".tv)))
  }

  "The type A [A:=D][D:=E]" should " beta reduce to E" in {
    red1s(red1s("A"
      , VarType("A")
      , VarType("D")
    ), "D", "E") should be(VarType("E"))
  }

  "The type A -> B [A:=D][D:=E]" should " beta reduce to E -> B" in {
    red1s(red1s("A".tv ->: "B".tv
      , VarType("A")
      , VarType("D")
    ), "D", "E") should be(ArrType2("E", "B"))
  }
}
