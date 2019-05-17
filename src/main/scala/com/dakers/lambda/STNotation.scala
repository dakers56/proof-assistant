package com.dakers.lambda

trait STNotation {

  implicit class TypedTerm(val t: UTTerm) {
    def :|(u: SimpleType): Statement = Statement(t, u)
  }

  implicit class TypedTermFromString(val s: String) {
    def :|(u: SimpleType): Statement = Statement(Var(s), u)
  }

  implicit class ArrowConv(val t1: SimpleType) {
    def ->:(t2: SimpleType): SimpleType = ArrType(t2, t1)
  }

  implicit class ArrConvLeft(val s: String) {
    def tv: VarType = VarType(s)
  }

  implicit def strConvVar(s: String): SimpleType = VarType(s)

}
