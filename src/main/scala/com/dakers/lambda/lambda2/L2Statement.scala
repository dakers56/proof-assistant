package com.dakers.lambda.lambda2

import com.dakers.lambda.stlc.{ArrType, STStatement, SimpleType, VarType}
import com.dakers.lambda.{App, UTTerm, Var}

case class L2Statement(utTerm: UTTerm, l2Type: *) {
  override def toString: String = utTerm.toString + ":" + l2Type.toString
}

/**
 * Marker trait that encapsulates pi-types and simple types*/
trait * {

}

object * {
  def red1s(fullType: *, oldType: *, newType: *): * = {
    fullType match {
      case VarType(x) => if (oldType == VarType(x)) newType else fullType
      case ArrType(s, t) => ArrType2(red1s(s, oldType, newType), red1s(t, oldType, newType))
      case ArrType2(s, t) => ArrType2(red1s(s, oldType, newType), red1s(t, oldType, newType))
    }
  }
}


case class π(l2Type: *, depType: SimpleType) {
  override def toString: String = s"π($depType:*.$l2Type)"
}

case class ArrType2(l2Type: *, l2Type1: *) extends *




