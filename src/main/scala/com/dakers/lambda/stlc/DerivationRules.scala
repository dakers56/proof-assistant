package com.dakers.lambda.stlc

import com.dakers.lambda._

object Judgement {
  val separator = "|-"
}

case class Judgement[T](gamma: DerivationContext[T], subject: T) {
  override def toString: String = gamma.toString + Judgement.separator + subject.toString
}

object VarRule {
  def apply(judgement: Judgement[STStatement]): Option[Judgement[STStatement]] = judgement.subject match {
    case STStatement(Var(x), sType) => if (judgement.gamma.stmts.contains(judgement.subject)) Some(judgement) else None
    case _ => None
  }

  object ApplRule {
    def apply(judgement1: Judgement[STStatement], judgement2: Judgement[STStatement]): Option[Judgement[STStatement]] = {
      if (judgement1.gamma != judgement2.gamma) {
        val gamma1 = judgement1.gamma
        val gamma2 = judgement2.gamma
        println(s"No match; provided judgements with different contexts. Judgement 1: $gamma1. Judgement 2: $gamma2.")
        None
      }
      else {
        judgement1.subject.sType match {
          case ArrType(s, t) => s match {
            case judgement2.subject.sType => Some(Judgement(judgement1.gamma, stlc.STStatement(App(judgement1.subject.term, judgement2.subject.term), t)))
            case _ => {
              println(s"Second statement was not of the expected type. STStatement 1: " + judgement1.subject.term + "; " + judgement2.subject.term)
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


  object AbstRule {
    def apply(judgement: Judgement[STStatement]): Option[Judgement[STStatement]] = {
      val last = if (judgement.gamma != Nil && judgement.gamma.stmts().size > 0) judgement.gamma.stmts.last else return None
      val first = if (judgement.gamma != Nil && judgement.gamma.stmts().size > 0) judgement.gamma.stmts.reverse.tail.reverse else return None
      last match {
        case STStatement(Var(x), s) => Some(Judgement(SimplyTypedDerivationContext(first), stlc.STStatement(Abst(judgement.subject.term, Var(x)), ArrType(last.sType, judgement.subject.sType))))
        case _ => {
          println("Judgement was not of expected form for abstraction: " + judgement)
          None
        }
      }
    }
  }

}