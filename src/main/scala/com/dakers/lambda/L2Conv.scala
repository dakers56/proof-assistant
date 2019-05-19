package com.dakers.lambda

import com.dakers.lambda.lambda2.{L2PiType, L2SimpleType, π}
import com.dakers.lambda.stlc.{SimpleType, VarType}

trait L2Conv {

  implicit def l2SimpleConv(t: SimpleType) = L2SimpleType(t)

  implicit def l2SimpleConv(t: String) = L2SimpleType(VarType(t))

  implicit def l2PiConv(t: π) = L2PiType(t)
}
