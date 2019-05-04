package com.dakers.lambda

abstract class Term

case class Var(varName: String) extends Term

case class App(v1: Var, v2: Var) extends Term

case class Arrow(v1: Var, v2: Var) extends Term
