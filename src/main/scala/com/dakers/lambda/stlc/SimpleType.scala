package com.dakers.lambda.stlc

import com.dakers.lambda.lambda2.L2Type

/**
 * Simple types are also used in L2.
 */
abstract class SimpleType extends L2Type

case class VarType(val v: String) extends SimpleType {
  override def toString: String = v
}


case class ArrType(val s: SimpleType, val t: SimpleType) extends SimpleType {
  val ArrStr = "->"

  override def toString: String = s"($s$ArrStr$t)"
}



