package com.dakers.lambda

import org.scalatest._


class TermTest extends FlatSpec with Matchers {

  def varTermX = Var("X")

  def varTermY = Var("Y")

  "An abstraction over a variable" should " add a variable to its list of bound variables " +
    "provided that the variable is not already bound" in {
    //Case: Abstracting over a variable
    val abstTerm = Abst(varTermX, varTermY)
    abstTerm.boundVar(varTermY) should be(true)
    abstTerm.boundVar(varTermX) should be(false)
    varTermX.boundVar.isEmpty should be(true)
    varTermY.boundVar.isEmpty should be(true)
  }

  "An abstraction over an application " should " add a variable to its list of bound variables " +
    "provided that the variable is not already bound" in {

    val appVar1 = Var("M")
    val appVar2 = Var("N")
    val appTerm = App(appVar1, appVar2)
    appTerm.boundVar(appVar1) should be(false)
    appTerm.boundVar(appVar2) should be(false)
    appVar1.boundVar.isEmpty should be(true)
    appVar2.boundVar.isEmpty should be(true)

  }

  "An abstraction over an abstraction " should " add a variable to its list of bound variables " +
    "provided that the variable is not already bound" in {

    //Create an abstraction term to test. Variable  "abstVarIn" should be immediately added to the list of bound variables.
    val abstTermIn = Abst(varTermX, varTermY)

    //Check that the variable being abstracted over is added to the list of bound variables but the variable for the original term is not
    // Creating local function to test same conditions as postconditions of abstracting over an abstraction term
    def yShouldBeBound = abstTermIn.boundVar(varTermY) should be(true)
    def xShouldBeBound = abstTermIn.boundVar(varTermX) should be(false)

    xShouldBeBound
    yShouldBeBound


    //Now abstract over the abstraction term
    val abstVarOut = Var(varTermX + "1") //Guarantee this is a new variable
    val abstTermOut = Abst(abstTermIn, abstVarOut)
    abstTermOut.boundVar(abstVarOut) should be(true)
    abstTermOut.boundVar(varTermY) should be(false)
    abstTermOut.boundVar(varTermX) should be(false)

    //Ensure original term is not affected
    xShouldBeBound
    yShouldBeBound

  }

}
