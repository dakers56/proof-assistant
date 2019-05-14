package com.dakers.lambda

abstract class Term(val free: Set[String], val bound: Set[String]) {
  val varNames = free union bound
}
