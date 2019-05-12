package com.dakers

import scala.collection.mutable.ListBuffer

package object lambda {

  val AbstOp = "/|"
  val AbstSep = "."

  var context = scala.collection.mutable.Map[String, UTTerm]()
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

  def +~(t: UTTerm): Unit = {
    context += (t.toString -> t)
  }

  def -~(t: UTTerm): Unit = {
    context -= t.toString
  }


  implicit class GetTerm(val k: String) {
    def ? = context.get(k).getOrElse(throw new RuntimeException(s"Term $k was not available in current context."))
  }


  implicit class ApplicationTerm(val t1: UTTerm) {
    def *(t2: UTTerm) = if (context.contains(t1.toString) && context.contains(t2.toString)) {
      val term = App(t1, t2)
      context += (term.toString -> term)
      term
    } else throw new RuntimeException(s"One of  $t1, $t2 was not yet declared")
  }

  implicit class TermTyper(val term: UTTerm) {
    def `:`[T <: SimpleType](sType: T): STTerm[T] = STTerm(term, sType)
  }


  def /|(s: String, t: UTTerm): Abst = if (varNames.contains(s.toString) && context.contains(t.toString)) {
    val term = Abst(t, Var(s))
    context += (term.toString -> term)
    term
  } else throw new RuntimeException(s"One of  $s, $t was not yet declared")


}