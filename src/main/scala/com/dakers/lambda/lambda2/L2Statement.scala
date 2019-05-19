package com.dakers.lambda.lambda2

import com.dakers.lambda.stlc.{STStatement, SimpleType}

case class L2Statement(term: STStatement, l2Type: L2Type) {
  override def toString: String = term + STStatement.TypeSep + l2Type
}

abstract class L2Type

case class L2SimpleType(t: SimpleType) extends L2Type

case class L2PiType(t: π) extends L2Type




