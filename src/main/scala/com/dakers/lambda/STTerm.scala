package com.dakers.lambda

case class STTerm(val term: UTTerm, val sType: SimpleType) {
  override def toString: String = term + STTerm.TypeSep + sType
}

object STTerm {
  val TypeSep: String = ":"
}