package com.dakers.lambda.stlc

import com.dakers.lambda.{SimpleType, UTTerm}

case class Statement(val term: UTTerm, val sType: SimpleType) {
  override def toString: String = term + Statement.TypeSep + sType
}

object Statement {
  val TypeSep: String = ":"
}