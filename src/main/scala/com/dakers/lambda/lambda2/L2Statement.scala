package com.dakers.lambda.lambda2

import com.dakers.lambda.UTTerm
import com.dakers.lambda.stlc.{ArrType, SimpleType, VarType}

case class L2Statement(utTerm: UTTerm, l2Type: *) {
  override def toString: String = utTerm.toString + ":" + l2Type.toString
}

/**
 * Marker trait that encapsulates pi-types and simple types*/

class * {
  override def toString: String = "*"
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


case class π(l2Type: *, depType: SimpleType) {
  override def toString: String = s"π($depType:*.$l2Type)"
}

/**
 * Type of all types.
 */
case object *** extends * {

}

case class ArrType2(l2Type: *, l2Type1: *) extends *

object π {


}




