package com.dakers.lambda

object TermUtils {
  def main(args: Array[String]): Unit = {
    println("Testing 'toString()' method for lambda terms.")
    println("A variable M is printed as '" + BVar("M") + "'")
    println("A application of a variable M to another variable N is printed as '" + App(BVar("M"), BVar("N")) + "'")
    println("An abstraction of M via N ais printed as '" + Abst(FVar("M"),BVar("N")))
    println("An abstraction of MN via N is printed as '" + Abst(App(FVar("M"), BVar("N")), BVar("N")) + "'")
    println("-----------------------")
  }
}
