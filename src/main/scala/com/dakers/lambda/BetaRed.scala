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

  def redexes(term: Term):List[App] = redexes(term, List())

  def redexes(term: Term, rdxs: List[App]): List[App] = {
    term match {
      case Var(_) => rdxs
      case App(Abst(t1, t2), t3) => term.asInstanceOf[App] :: redexes(t1, rdxs) ::: redexes(t3, rdxs)
      case App(t1, t2) => redexes(t1, rdxs) ::: redexes(t2, rdxs)
      case Abst(t1, t2) => redexes(t1, rdxs)
    }
  }

}
