package com.dakers.lambda

object AlphaConv {

  /**
   * Returns a term where x is renamed to y.
   * Checks to make sure that the potential new variable does not already occur in original term.
   * Always checks if variable to rename occurs in term for efficiency's sake.
   *
   * @param from Term to rename
   * @param x    Variable to replace
   * @param y    New variable name
   * @return If alpha convertible, term with x replaced by y. Otherwise an exception is thrown.
   * @throws RuntimeException If the new variable already exists in the term.
   */
  def Mxy(from: UTTerm, x: String, y: String): UTTerm = {
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
  def isInTerm(term: UTTerm, y: String): Boolean = {
    return term.varNames(y)
  }


  /**
   * Implements the substitution operation.
   * Definition 1.6.1​(Substitution) (1a) x[x := N] ≡ N, (1b) y[x := N] ≡ y if x ≢ y, (2) (PQ)[x := N] ≡ (P[x := N])(Q[x := N]), (3) (λy . P)[x := N] ≡ λz . (Py→z [x := N]), if λz . Py→z is an α-variant of λy . P such that z ∉ FV (N).
   *
   * Nederpelt, Rob. Type Theory and Formal Proof: An Introduction (Kindle Locations 840-845). Cambridge University Press. Kindle Edition.
   *
   * @param M
   * @param N
   * @param x
   * @return
   */
  def subst(M: UTTerm, N: UTTerm, x: String): UTTerm = {
    if(M.bound(x)) {
      throw new RuntimeException(s"$x was already a bound variable in $M")
    }
    var intersection = M.free intersect (N.free)
    intersection = M.bound intersect (N.bound)
    if (!intersection.isEmpty) {
      throw new RuntimeException(s"$M and $N had bound variables in common: $intersection")
    }
    M match {
      case Var(v) => if (v == x) N else M
      case App(t1, t2) => App(subst(t1, N, x), subst(t2, N, x))
      case Abst(m, z) => if (z == x) throw new RuntimeException(s"Cannot substitute for bound variable $z in $m") else Abst(subst(m, N, x), z)
    }

  }
}
