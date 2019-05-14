package com.dakers.lambda

trait STTermNotation {

  implicit class TypedTerm(val t: UTTerm) {
    def :|(u: SimpleType): STTerm[SimpleType] = STTerm(t, u)
  }

  implicit class TypedTermFromString(val s: String) {
    def :|(u: SimpleType): STTerm[SimpleType] = STTerm(Var(s), u)
  }

  implicit class ArrowConv(val t1: SimpleType) {
    def ->(t2: SimpleType): ArrType[SimpleType] = ArrType(t1, t2)
  }

  implicit class ArrConvLeft(val s: String) {
    def vt : VarType = VarType(s)
  }

  implicit def strConvVar(s: String): VarType = VarType(s)


}
