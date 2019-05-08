package com.dakers.lambda

object BetaRed {

  /**
   * Performs 1 step beta-reduction
   *
   * @param term Term to beta reduce
   * @param x    name of variable
   * @return If term is a redex (i.e. (/|x.M)N), then M[x:=N]), otherwise the term itself.
   */
  def red1s(term: Term, x: String): Term = {
    term match {
      case App(Abst(m, Var(x)), n) =>
        AlphaConv.subst(m, n, x)
      case a: Abst => a
      case _ => println(s"$term was already in beta-normal form"); term
    }
  }

  /**
   * Calculates all redexes in a given term. Pattern matches for terms of form (/|x.M)N).
   * All redexes are application terms, so a list of {@}
   * @param term
   * @param rdxs
   * @return
   */
  def redexes(term: Term, rdxs: List[App]): List[App] = {
    term match {
      case Var(_) => rdxs
      case App(Abst(t1, t2), t3) => term.asInstanceOf[App] :: redexes(t1, rdxs) ++ redexes(t3, rdxs)
      case App(t1, t2) => redexes(t1, rdxs) ++ redexes(t2, rdxs)
      case Abst(t1, t2) => redexes(t1, rdxs)
    }
  }

}
