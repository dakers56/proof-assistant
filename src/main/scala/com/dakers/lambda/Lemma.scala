package com.dakers.lambda

import com.dakers.lambda.stlc.{Judgement, Statement}

object Subcontext {

  def isSubcontext(g1: List[Statement], g2: List[Statement]): Boolean = {
    g1 match {
      case Nil => true
      case h :: t => g2 match {
        case Nil => false
        case h1 :: t1 => if (h == h1) isSubcontext(t, t1) else false
      }
    }
  }

  def isSubcontext(d1: DerivationContext[Statement], d2: DerivationContext[Statement]): Boolean = isSubcontext(d1.stmts(), d2.stmts())

}

object Thinning {
  def apply[T](j: Judgement[Statement], g: SimplyTypedDerivationContext): Option[Judgement[Statement]] = {
    Subcontext.isSubcontext(j.gamma, g) match {
      case true => Some(Judgement(g, j.subject))
      case false => None
    }
  }
}
  object Condensing {
    def apply(j: Judgement[Statement]): Judgement[Statement] = {
      stlc.Judgement(SimplyTypedDerivationContext(j.gamma.proj(j.subject.term.free)), j.subject)
    }
  }

  object Permutation {
    def apply(j: Judgement[Statement], perm: SimplyTypedDerivationContext): Option[Judgement[Statement]] = {
      if (j.gamma.stmts().toSet == perm.stmts().toSet)  return Some(stlc.Judgement(perm, j.subject))
      val stmts = j.gamma.stmts()
      println(s"$perm was not a permutation of $stmts")
      None
    }
  }



