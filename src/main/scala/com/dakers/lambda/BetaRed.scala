package com.dakers.lambda

object BetaRed {

  def red1s(term: Term, x: String): Term = {
    term match {
      case App(Abst(m, Var(x)), n) =>
        AlphaConv.subst(m, n, x)
      case a: Abst => a
      case _ => println(s"$term was already in beta-normal form"); term
    }
  }
}
