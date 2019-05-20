package com.dakers.lambda.lambda2

import com.dakers.lambda.stlc.{ArrType, Judgement}
import com.dakers.lambda.{App, STNotation, UTNotation, Var}
import π.subst;

object VarRule {
  def apply(judgement: L2Judgement): Option[L2Judgement] = judgement.s.get match {
    case L2Statement(Var(x), sType) => if (judgement.l2Context.stmtContext.stmts().contains(judgement.s.get)) Some(judgement) else None
    case _ => None
  }
}


object ApplRule {
  def apply(judgement1: L2Judgement, judgement2: L2Judgement): Option[L2Judgement] = {
    if (judgement1.l2Context != judgement2.l2Context) {
      val gamma1 = judgement1.l2Context
      val gamma2 = judgement2.l2Context
      println(s"No match; provided judgements with different contexts. Judgement 1: $gamma1. Judgement 2: $gamma2.")
      None
    }
    else {
      val case1 = judgement2.s.get.l2Type
      judgement1.s.get.l2Type match {
        case ArrType(s, t) => s match {
          case `case1` => Some(L2Judgement(judgement1.l2Context, L2Statement(App(judgement1.s.get.utTerm, judgement2.s.get.utTerm), t)))
          case _ => {
            println(s"Second statement was not of the expected type. L2Statement 1: " + judgement1.s.get.l2Type + "; " + judgement2.s.get.utTerm)
            None
          }
        }
        case _ => {
          println("First statement did was not an arrow type: " + judgement1.s.get)
          None
        }
      }
    }
  }
}

object Form2 extends L2Notation with STNotation with UTNotation {
  def apply(judgement: Judgement[L2Statement]): Option[Judgement[L2Statement]] = {
    val |- = judgement.gamma

    if (judgement.subject.utTerm.free.filterNot(v => judgement.gamma.stmts().contains(v)).isEmpty) {
      Some(Judgement(judgement.gamma, judgement.subject.utTerm :|| *()))
    }
    None
  }
}

//object Appl2 extends L2Notation with STNotation with UTNotation {
//  def apply(m: L2Judgement, n: L2Judgement): Option[Judgement[L2Statement]] = {
//    if (m.l2Context != n.gamma) {
//      println("Contexts were not the same")
//      None
//    }
//    m.subject.l2Type match {
//      case π(l2, vt) => Some(Judgement(m.gamma, L2Statement(m.subject.utTerm * n.subject.utTerm, subst(m.subject.l2Type, vt, n.subject.l2Type))))
//      case _ => {
//        println("First term given to Appl2 was not a pi type")
//        None
//      }
//    }
//  }
//}
