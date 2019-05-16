package com.dakers.lambda

import org.scalatest.{FlatSpec, Matchers}

class DerivationContextTest extends FlatSpec with Matchers with STTermNotation with UTTermNotation {

  //Basic tests for empty contexts
  "An empty untyped context" should " have no variable names and no statements in it" in {
    UntypedDerivationContext().stmts() should be(empty)
  }

}
