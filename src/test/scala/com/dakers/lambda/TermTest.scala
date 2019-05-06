package com.dakers.lambda

import org.scalatest._


class TermTest extends FlatSpec with Matchers {

  val varNameX = "X"
  val varNameY = "Y"

  def varTermX = Var(varNameX)

  def varTermY = Var(varNameY)

  "An abstraction over a variable" should " add a variable to its list of bound variables " +
    "provided that the variable is not already bound" in {
    val abstTerm = Abst(varTermX, varTermY)
    abstTerm.free() should be(Set(varNameX))
    abstTerm.bound() should be(Set(varNameY))
  }

  "An abstraction over an application " should " add a variable to its list of bound variables " +
    "provided that the variable is not already bound" in {
    val appTerm = App(varTermX, varTermY)
    appTerm.free() should be(Set(varNameX, varNameY))
    appTerm.bound() should be(Set.empty)
  }

  "An abstraction over an abstraction " should " add a variable to its list of bound variables " +
    "provided that the variable is not already bound" in {

    //Create an abstraction term to test. Variable  "abstVarIn" should be immediately added to the list of bound variables.
    val abstTermIn = Abst(varTermX, varTermY)

    //Check that the variable being abstracted over is added to the list of bound variables but the variable for the original term is not
    // Creating local function to test same conditions as postconditions of abstracting over an abstraction term


    def yShouldBeBoundInOriginal = abstTermIn.bound() should be(Set(varNameY))

    def xShouldBeFreeInOriginal = abstTermIn.free() should be(Set(varNameX))

    yShouldBeBoundInOriginal
    xShouldBeFreeInOriginal


    //Now abstract over the abstraction term
    val abstVarNameOut = varNameX + "1" //Guarantee this is a new variable
    val abstVarOut = Var(abstVarNameOut)
    val abstTermOut = Abst(abstTermIn, abstVarOut)
    abstTermOut.free() should be(Set(varNameX))
    abstTermOut.bound() should be(Set(varNameY, abstVarNameOut))

    //Ensure original term is not affected
    yShouldBeBoundInOriginal
    xShouldBeFreeInOriginal

  }

}
