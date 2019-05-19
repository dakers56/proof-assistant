package com.dakers.lambda

import com.dakers.lambda.stlc.Statement

trait STNotation {

  implicit class TypedTerm(val t: UTTerm) {
    def :|(u: SimpleType): Statement = stlc.Statement(t, u)
  }

  implicit class TypedTermFromString(val s: String) {
    def :|(u: SimpleType): Statement = stlc.Statement(Var(s), u)
  }

  implicit class ArrowConv(val t1: SimpleType) {
    def ->:(t2: SimpleType): ArrType = ArrType(t2, t1)
  }

  implicit class ArrConvLeft(val s: String) {
    def tv: VarType = VarType(s)
  }

  implicit def strConvVar(s: String): SimpleType = VarType(s)

}
