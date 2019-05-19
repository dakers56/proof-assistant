package com.dakers.lambda.stlc

import com.dakers.lambda.UTTerm

case class STStatement(val term: UTTerm, val sType: SimpleType) {
  override def toString: String = term + STStatement.TypeSep + sType
}

object STStatement {
  val TypeSep: String = ":"
}