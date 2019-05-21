package com.dakers.lambda.lambda2

import com.dakers.lambda._
import com.dakers.lambda.lambda2.π.subst
import com.dakers.lambda.stlc.{ArrType, Judgement, VarType};

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

object Appl2 extends L2Notation with STNotation with UTNotation {
  def apply(m: L2Judgement, n: L2Judgement): Option[L2Judgement] = {
    if (m.l2Context != n.l2Context) {
      println("Contexts were not the same")
      None
    }
    m.s.get.l2Type match {
      case π(l2, vt) => Some(L2Judgement(m.l2Context, L2Statement(m.s.get.utTerm * n.s.get.utTerm, subst(m.s.get.l2Type, vt, n.s.get.l2Type))))
      case _ => {
        println("First term given to Appl2 was not a pi type")
        None
      }
    }
  }
}


object AbstRule {
  def apply(judgement: L2Judgement, varName: String, varType: VarType): Option[L2Judgement] = {
    /**
     * If there are no variable declarations in the context, we cannot get a term of the form x:T to abstract with.
     * So long as such a term exists in the context, we may permute the context to get it such that the variable
     * declaration comes last.
     */
    val l = judgement.l2Context.stmtContext.stmts()
    if (l.isEmpty) return None
    if (judgement.s.isEmpty) return None


    //Check if there is a variable declaration
    val searchFor = L2Statement(Var(varName), varType)
    val varDecl: Option[L2Statement] = judgement.l2Context.stmtContext.stmts().filter(s => s == searchFor).headOption
    if (varDecl.isEmpty) {
      println("No such variable declaration was in the context: " + searchFor)
      return None
    }

    val M = judgement.s.get
    val abstVar: Var = varDecl.get.utTerm.asInstanceOf[Var]
    val abstVarType: VarType = varDecl.get.l2Type.asInstanceOf[VarType]
    val abstTermUt: Abst = Abst(M.utTerm, abstVar)
    val newStmts: List[L2Statement] = judgement.l2Context.stmtContext.stmts().filterNot(s => s == varDecl.get)
    val ctx: L2Context = L2Context(newStmts, judgement.l2Context.typeContext.stmts())
    val arrType = ArrType2(abstVarType, judgement.s.get.l2Type)
    val st: Option[L2Statement] = Some(L2Statement(abstTermUt, arrType))
    Some(L2Judgement(ctx, st, None))
  }


}

object AbstRule2 {
  def apply(judgement: L2Judgement, varName: String): Option[L2Judgement] = {
    /**
     * If there are no variable declarations in the context, we cannot get a term of the form x:T to abstract with.
     * So long as such a term exists in the context, we may permute the context to get it such that the variable
     * declaration comes last.
     */
    if (judgement.s.isEmpty) {
      println("No statement was provided for the judgement")
      return None
    }
    val l = judgement.l2Context.typeContext.stmts()
    if (l.isEmpty) return None
    if (judgement.s.isEmpty) return None


    //Check if there is a variable declaration
    val searchFor: L2TypeDecl = L2TypeDecl(varName)
    val varDecl: Option[L2TypeDecl] = judgement.l2Context.typeContext.stmts().find(s => s == searchFor)
    if (varDecl.isEmpty) {
      println("No such variable declaration was in the context: " + searchFor)
      return None
    }

    val newStmts: List[L2TypeDecl] = judgement.l2Context.typeContext.stmts().filterNot(s => s == varDecl.get)
    val ctx: L2Context = L2Context(judgement.l2Context.stmtContext.stmts(), newStmts)
    val piType = π(judgement.s.get.l2Type, VarType(varName))
    val st: Option[L2Statement] = Some(L2Statement(judgement.s.get.utTerm, piType))
    Some(L2Judgement(ctx, st, None))
  }


}