package com.dakers.lambda.lambda2

import com.dakers.lambda.stlc.{ArrType, Judgement}
import com.dakers.lambda.{App, Var}

object VarRule {
  def apply(judgement: Judgement[L2Statement]): Option[Judgement[L2Statement]] = judgement.subject match {
    case L2Statement(Var(x), sType) => if (judgement.gamma.stmts.contains(judgement.subject)) Some(judgement) else None
    case _ => None
  }
}


object ApplRule {
  def apply(judgement1: Judgement[L2Statement], judgement2: Judgement[L2Statement]): Option[Judgement[L2Statement]] = {
    if (judgement1.gamma != judgement2.gamma) {
      val gamma1 = judgement1.gamma
      val gamma2 = judgement2.gamma
      println(s"No match; provided judgements with different contexts. Judgement 1: $gamma1. Judgement 2: $gamma2.")
      None
    }
    else {
      judgement1.subject.l2Type match {
        case ArrType(s, t) => s match {
          case judgement2.subject.l2Type => Some(Judgement(judgement1.gamma, L2Statement(App(judgement1.subject.utTerm, judgement2.subject.utTerm), t)))
          case _ => {
            println(s"Second statement was not of the expected type. L2Statement 1: " + judgement1.subject.l2Type + "; " + judgement2.subject.utTerm)
            None
          }
        }
        case _ => {
          println("First statement did was not an arrow type: " + judgement1.subject)
          None
        }
      }
    }
  }
}
