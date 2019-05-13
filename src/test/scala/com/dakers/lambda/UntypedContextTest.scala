package com.dakers.lambda

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable.Set

class UntypedContextTest extends FlatSpec with Matchers with Notation {

  "An empty context" should "have no variables" in {
    UntypedContext(Set[String]()).varNames should be(empty)
  }

  "Creating a context with one variable " should "yield a context containing just that variable" in {
    val varName = "s"
    UntypedContext(Set[String](varName)).varNames should be(Set(varName))
  }

  "Adding a variable to a context with no variables " should "yield a context containing just that variable" in {
    val varName = "s"
    (UntypedContext(Set[String]()) + "s").varNames should be(Set(varName))
  }

  "Adding a variable x to a context with a variable y" should "update the original context to contain both x and y" in {
    val origVar = "y"
    val newVar = "x"
    val origContext = UntypedContext(Set[String](origVar))
    (origContext + newVar).varNames should be(Set(origVar, newVar))
  }

  "Adding a variable to a context already containing that variable " should "throw an exception" in {
    val varName = "s"
    val context = UntypedContext(Set[String](varName))
    intercept[RuntimeException](context + varName)
  }

  "Removing an element from an empty context" should "yield an empty context" in {
    (UntypedContext(Set[String]()) - "x") should be(UntypedContext(Set[String]()))
  }

  "Removing a variable x from a context containing only a variable x" should "yield an empty context" in {
    (UntypedContext(Set[String]("x")) - "x") should be(UntypedContext(Set[String]()))
  }

  "Removing a variable x from a context containing only a variable x and a variable y" should "yield a context containing only x" in {
    (UntypedContext(Set[String]("x", "y")) - "y") should be(UntypedContext(Set[String]("x")))
  }
  "Removing a variable y from a non-empty context that does not contain y" should "yield the original context" in {
    val orig = UntypedContext(Set[String]("x"))
    (orig - "y") should be(orig)
  }

}
