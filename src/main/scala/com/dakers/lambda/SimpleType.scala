package com.dakers.lambda

abstract class SimpleType

case class VarType(val v: String) extends SimpleType {
  override def toString: String = v
}


case class ArrType(val s: SimpleType, val t: SimpleType) extends SimpleType {
  val ArrStr = "->"

  override def toString: String = s"($s$ArrStr$t)"
}



