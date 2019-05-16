package com.dakers.lambda

object Judgement {
  val separator = "|-"
}

case class Judgement[T](gamma: DerivationContext[T], subject: T) {
  override def toString: String = gamma.toString + Judgement.separator + subject.toString
}

object VarRule {
  def apply(judgement: Judgement[Statement]): Option[Judgement[Statement]] = judgement.subject match {
    case Statement(Var(x), sType) => if (judgement.gamma.stmts.contains(judgement.subject)) Some(judgement) else None
    case _ => None
  }

  object ApplRule {
    def apply(judgement1: Judgement[Statement], judgement2: Judgement[Statement]): Option[Judgement[Statement]] = {
      if (judgement1.gamma != judgement2.gamma) {
        val gamma1 = judgement1.gamma
        val gamma2 = judgement2.gamma
        println(s"No match; provided judgements with different contexts. Judgement 1: $gamma1. Judgement 2: $gamma2.")
        None
      }
      else {
        judgement1.subject.sType match {
          case ArrType(s, t) => s match {
            case judgement2.subject.sType => Some(Judgement(judgement1.gamma, Statement(App(judgement1.subject.term, judgement2.subject.term), t)))
            case _ => {
              println(s"Second statement was not of the expected type. Statement 1: " + judgement1.subject.term + "; " + judgement2.subject.term)
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
    def apply(judgement: Judgement[Statement]): Option[Judgement[Statement]] = {
      judgement.gamma.stmts.reverse match {
        case Statement(Var(x), s) :: gamma => Some(Judgement(SimplyTypedDerivationContext(gamma), Statement(Abst(judgement.subject.term, Var(x)), s)))
        case _ => {
          println("Judgement was not of expected form for abstraction: " + judgement)
          None
        }
      }
    }
  }

}