package com.dakers.lambda

abstract class Term

case class Var(varName: String) extends Term

case class App(v1: Term, v2: Term) extends Term

case class Arrow(v1: Term, v2: Term) extends Term
