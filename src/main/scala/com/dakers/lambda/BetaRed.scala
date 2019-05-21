package com.dakers.lambda

object BetaRed {

  def red1s(term: UTTerm, x: String): UTTerm = {
    term match {
      case App(Abst(m, Var(x)), n) =>
        AlphaConv.subst(m, n, x)
      case Abst(t, u) => Abst(AlphaConv.subst(t, term, x), u)
      case Var(v) => if (x == v) Var(v) else term
    }
  }

  def redexes(term: UTTerm): List[App] = redexes(term, List())

  def redexes(term: UTTerm, rdxs: List[App]): List[App] = {
    term match {
      case Var(_) => rdxs
      case App(Abst(t1, t2), t3) => term.asInstanceOf[App] :: redexes(t1, rdxs) ::: redexes(t3, rdxs)
      case App(t1, t2) => redexes(t1, rdxs) ::: redexes(t2, rdxs)
      case Abst(t1, t2) => redexes(t1, rdxs)
    }
  }

}
