package com.dakers.lambda

trait STTermNotation {

  implicit class TypedTerm(val t: UTTerm) {
    def :|(u: SimpleType) = STTerm(t, u)
  }

}
