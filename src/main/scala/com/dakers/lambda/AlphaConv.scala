package com.dakers.lambda

object AlphaConv {

  /**
   * Returns a term which is alpha-convertible from the given term.
   * Checks to make sure that the potential new variable does not already occur in original term.
   * Always checks if variable to rename occurs in term for efficiency's sake.
   *
   * @param from Term to rename
   * @param x    Variable to replace
   * @param y    New variable name
   * @return If alpha convertible, term with x replaced by y. Otherwise an exception is thrown.
   * @throws RuntimeException If the new variable already exists in the term.
   */
  def Mxy(from: Term, x: String, y: String): Term = {
    if (!isInTerm(from, x)) {
      println(s"Variable $x was not in term $from")
      return from
    }
    if (isInTerm(from, y)) throw new RuntimeException(s"Variable $y occurred either free or bound in $from.")
    from match {
      case Var(_) => Var(y)
      case Abst(t1, t2) => Abst(Mxy(t1, x, y), Mxy(t2, x, y).asInstanceOf[Var])
      case App(t1, t2) => App(Mxy(t1, x, y), Mxy(t2, x, y))
    }
  }

  /**
   * @param term Term to check for given variable
   * @param y    Name of variable
   * @return True if y is a candidate for renaming ANY variable in term
   */
  def isInTerm(term: Term, y: String): Boolean = {
    return term.varNames(y)
  }
}
