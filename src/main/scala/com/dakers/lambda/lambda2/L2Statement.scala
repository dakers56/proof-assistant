package com.dakers.lambda.lambda2

import com.dakers.lambda.stlc.{ArrType, VarType}
import com.dakers.lambda.{FBVars, UTTerm}

case class L2Statement(utTerm: UTTerm, l2Type: *) {
  override def toString: String = utTerm.toString + ":" + l2Type.toString
}

/**
 * Represents the type of all types*/

class * extends FBVars {
  override def toString: String = "*"

  override def free(): Set[*] = Set()

  override def bound(): Set[*] = Set()
}

object * {
  def apply(): * = new *()

  def red1s(fullType: *, oldType: *, newType: *): * = {
    fullType match {
      case VarType(x) => if (oldType == VarType(x)) newType else fullType
      case ArrType(s, t) => ArrType2(red1s(s, oldType, newType), red1s(t, oldType, newType))
      case ArrType2(s, t) => ArrType2(red1s(s, oldType, newType), red1s(t, oldType, newType))
    }
  }

  override def toString: String = "*"
}


case class π(l2Type: *, toBind: VarType) extends * {
  override def toString: String = s"π($toBind:*.$l2Type)"

  override def free(): Set[*] = l2Type.free() -- toBind.free

  override def bound(): Set[*] = l2Type.bound() + toBind
}

/**
 * Type of all types.
 */
case object *** extends * {}

case class ArrType2(l2Type: *, l2Type1: *) extends * {
  override def free(): Set[*] = l2Type.free() ++ l2Type1.free()

  override def bound(): Set[*] = l2Type.bound() ++ l2Type1.bound()
}




