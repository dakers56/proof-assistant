package com.dakers.lambda.stlc

import com.dakers.lambda.lambda2.*

/**
 * Simple types are also used in L2.
 */
abstract class SimpleType extends *

case class VarType(v: String) extends SimpleType {
  override def toString: String = v

  override def free(): Set[*] = Set(this)

  override def bound(): Set[*] = Set()
}


case class ArrType(s: SimpleType, t: SimpleType) extends SimpleType {
  val ArrStr = "->"

  override def toString: String = s"($s$ArrStr$t)"

  override def free = Set() ++ s.free() ++ t.free()

  override def bound = Set() ++ s.bound() ++ t.bound()
}



