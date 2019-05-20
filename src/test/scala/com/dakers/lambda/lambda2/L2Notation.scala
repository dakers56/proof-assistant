package com.dakers.lambda.lambda2

import com.dakers.lambda.{UTTerm, Var, stlc}
import com.dakers.lambda.stlc.{STStatement, SimpleType, VarType}

trait L2Notation {

  implicit class L2TypedTerm(val t: UTTerm) {
    def :||(u: *): L2Statement = L2Statement(t, u)
  }

  implicit class L2TypedTermString(val t: String) {
    def :||(u: *): L2Statement = L2Statement(Var(t), u)
  }

}
