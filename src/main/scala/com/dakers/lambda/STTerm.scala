package com.dakers.lambda

case class STTerm(val term: UTTerm, val sType: SimpleType) extends Term(term.free, term.bound) {
  override def toString: String = term + STTerm.TypeSep + sType
}

object STTerm {
  val TypeSep: String = ":"
}