package com.dakers.lambda

case class STTerm[T <: SimpleType](val term: UTTerm, val sType: T) {
  override def toString: String = term + STTerm.TypeSep + sType
}

object STTerm {
  val TypeSep: String = ":"
}