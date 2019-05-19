package com.dakers.lambda

/**
 *
 * @param term    Same as an untyped term, but the terms are to be interpreted as types.
 * @param depType The typed bound by the π binder
 */
case class π(term: UTTerm, depType: SimpleType) {
  override def toString: String = s"π($depType:*.$term)"
}

object test {

  def main(args: Array[String]): Unit = {
    println(π(Var("x"), VarType("v")))
  }
}