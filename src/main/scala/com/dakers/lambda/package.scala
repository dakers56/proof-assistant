package com.dakers

import scala.collection.mutable.ListBuffer

package object lambda {

  val AbstOp = "/|"
  val AbstSep = "."

  var context = scala.collection.mutable.Map[String, Term]()
  var varNames = ListBuffer[String]()

  implicit class DeclareVariable(val v: String) {
    def decl(): Var = {
      val newVar = Var(v);
      context += (v -> newVar)
      if (varNames.contains(v)) throw new RuntimeException(s"Variable name $v was already used.")
      varNames += v
      newVar
    }
  }

  def +~(t: Term): Unit = {
    context += (t.toString -> t)
  }

  def -~(t: Term): Unit = {
    context -= t.toString
  }


  implicit class GetTerm(val k: String) {
    def ? = context.get(k).getOrElse(throw new RuntimeException(s"Term $k was not available in current context."))
  }


  implicit class ApplicationTerm(val t1: Term) {
    def *(t2: Term) = if (context.contains(t1.toString) && context.contains(t2.toString)) App(t1, t2) else throw new RuntimeException(s"One of  $t1, $t2 was not yet declared")
  }


  def /|(s: String, t: Term): Abst = if (varNames.contains(s.toString) && context.contains(t.toString)) Abst(t, Var(s)) else throw new RuntimeException(s"One of  $s, $t was not yet declared")


}