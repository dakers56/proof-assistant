package com.dakers.lambda

import org.scalatest._


class TermTest extends FlatSpec with Matchers {

  "An abstraction over a variable" should " add a variable to its list of bound variables " +
    "provided that the variable is not already bound" in {

    //Case: Abstracting over a variable
    val abstVar = Var("x")
    val termVar = Var("y")
    val abstTerm = Abst(termVar, abstVar)
    abstTerm.boundVar(abstVar) should be(true)
    abstTerm.boundVar(termVar) should be(false)
    abstVar.boundVar.isEmpty should be(true)
    termVar.boundVar.isEmpty should be(true)


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
    val termVarIn = Var("M")
    val abstVarIn = Var("N")
    val abstTermIn = Abst(termVarIn, abstVarIn)

    //Check that the variable being abstracted over is added to the list of bound variables but the variable for the original term is not
    abstTermIn.boundVar(termVarIn) should be(false)
    abstTermIn.boundVar(abstVarIn) should be(true)

    //Now abstract over the abstraction term
    val abstVarOut = Var(abstVarIn + "1") //Guarantee this is a new variable
    val abstTermOut = Abst(abstTermIn, abstVarOut)
    abstTermOut.boundVar(abstVarOut) should be(true)
    abstTermOut.boundVar(abstVarIn) should be(false)
    //Ensure original term is not affected
    abstTermIn.boundVar(termVarIn) should be(false)
    abstTermIn.boundVar(abstVarIn) should be(true)

  }

}
