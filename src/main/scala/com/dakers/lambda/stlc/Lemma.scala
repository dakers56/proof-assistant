package com.dakers.lambda.stlc

import com.dakers.lambda.{DerivationContext, SimplyTypedDerivationContext, stlc}

object Subcontext {

  def isSubcontext(g1: List[STStatement], g2: List[STStatement]): Boolean = {
    g1 match {
      case Nil => true
      case h :: t => g2 match {
        case Nil => false
        case h1 :: t1 => if (h == h1) isSubcontext(t, t1) else false
      }
    }
  }

  def isSubcontext(d1: DerivationContext[STStatement], d2: DerivationContext[STStatement]): Boolean = isSubcontext(d1.stmts(), d2.stmts())

}

object Thinning {
  def apply[T](j: Judgement[STStatement], g: SimplyTypedDerivationContext): Option[Judgement[STStatement]] = {
    Subcontext.isSubcontext(j.gamma, g) match {
      case true => Some(Judgement(g, j.subject))
      case false => None
    }
  }
}
  object Condensing {
    def apply(j: Judgement[STStatement]): Judgement[STStatement] = {
      stlc.Judgement(SimplyTypedDerivationContext(j.gamma.proj(j.subject.term.free)), j.subject)
    }
  }

  object Permutation {
    def apply(j: Judgement[STStatement], perm: SimplyTypedDerivationContext): Option[Judgement[STStatement]] = {
      if (j.gamma.stmts().toSet == perm.stmts().toSet)  return Some(stlc.Judgement(perm, j.subject))
      val stmts = j.gamma.stmts()
      println(s"$perm was not a permutation of $stmts")
      None
    }
  }



