package com.dakers.lambda

import com.dakers.lambda.TermUtils.checkVars


trait Notation {

  implicit class AppTermTerm(val t1: UTTerm) {
    def *(t2: UTTerm) = {
      checkVars(t1, t2)
      App(t1, t2)
    }

    def *(t2: String): App = t1 * Var(t2)
  }

  implicit class AppStringTerm(val s1: String) {
    def *(t2: UTTerm): App = {
      val t1 = Var(s1)
      checkVars(t1, t2)
      App(t1, t2)
    }

    def *(s2: String): App = {
      val t1 = Var(s1)
      val t2 = Var(s2)
      checkVars(t1, t2)
      App(t1, t2)
    }
  }

  def /|(s: String, t: UTTerm): Abst = if (t.bound.contains(s.toString)) {
    throw new RuntimeException(s"Cannot abstract over variable $s already bound in term $t")
  } else Abst(t, Var(s))

  def /|(s: String, t: String): Abst = /|(s, Var(t))

}

object Notation {
  def strToVar(s: String): Var = {
    Var(s)
  }
}