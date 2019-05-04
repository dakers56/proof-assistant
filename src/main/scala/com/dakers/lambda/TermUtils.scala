package com.dakers.lambda

object TermUtils {

  def print(t : Term) : String = t match {
    case Var(v) => v
    case App(v1, v2) => print(v1) + print(v2)
    case Arrow(v1, v2) => print(v1) + " -> " + print(v2)
  }

  def main(args: Array[String]): Unit = {
    println("Testing 'print()' method for lambda terms.")
    println("A variable M is printed as '" + print(Var("M")) +"'")
    println("A application of a variable M to another variable N is printed as '" + print(App(Var("M"), Var("N"))) +"'")
    println("An arrow type from a variable M to another variable N is printed as '" + print(Arrow(Var("M"), Var("N"))) +"'")
  }
}
