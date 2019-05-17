package com.dakers.lambda


trait UTNotation {

  implicit class AppTermTerm[T <: UTTerm](val t1: T) {
    def *(t2: UTTerm): App = {
      App(t1, t2)
    }

    def *(t2: String): App = t1 * Var(t2)
  }

  implicit class AppStringTerm(val s1: String) {
    def *(t2: UTTerm): App = {
      val t1 = Var(s1)
      App(t1, t2)
    }

    def *(s2: String): App = {
      val t1 = Var(s1)
      val t2 = Var(s2)
      App(t1, t2)
    }
  }

  def /|(s: String, t: UTTerm): Abst = if (t.bound.contains(s.toString)) {
    throw new RuntimeException(s"Cannot abstract over variable $s already bound in term $t")
  } else Abst(t, Var(s))

  def /|(s: String, t: String): Abst = /|(s, Var(t))

  implicit def varConv(s: String): UTTerm = Var(s)

}

object UTNotation {
  def strToVar(s: String): UTTerm = {
    Var(s)
  }
}