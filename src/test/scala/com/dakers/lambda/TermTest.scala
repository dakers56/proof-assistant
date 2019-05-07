package com.dakers.lambda

import org.scalatest._


class TermTest extends FlatSpec with Matchers {

  val varNameX = "X"
  val varNameY = "Y"

  def varTermX = Var(varNameX)

  def varTermY = Var(varNameY)

  "A variable" should "never have an element in its list of bound elements" in {
    varTermX.free should be(Set(varNameX))
    varTermX.bound should be(Set.empty)

    varTermY.free should be(Set(varNameY))
    varTermY.bound should be(Set.empty)

    //Trying a host of variable names to try to "break" computation of free variables
    val varNames = Array("1", "2", "::", "ABC", "Z", "98asdasf!@#$", "F4*&")
    for (name <- varNames) {
      val term = Var(name)
      term.free should be(Set(name))
      term.bound should be(Set.empty)
    }
  }

  "An application of one term to another with no variable names in common" should " have free variables equal to the union of " +
    "its input terms free variables, and likewise for bound variables" in {

    val inputList = List((varTermX, varTermY), (Var(varTermX + "1"), Var(varTermY + "2")), (App(Var("Z"), Var("A")), Var("B")),
      (Abst(Var("Z"), Var("Y")), Var("A")))
    for ((t1, t2) <- inputList) {
      val appTerm = App(t1, t2)
      appTerm.free should be(t1.free ++ t2.free)
      appTerm.bound should be(t1.bound ++ t2.bound)
    }
  }

  "An application of one term to another with variable names in common" should " throw an exception" in {


    intercept[RuntimeException](
      App(varTermX, varTermX)
    )
    intercept[RuntimeException](
      App(varTermY, varTermY)
    )

    intercept[RuntimeException](
      App(varTermX, App(varTermY, varTermY))
    )

    intercept[RuntimeException](
      App(varTermX, App(varTermX, varTermY))
    )
  }

  "An abstraction over a variable" should " add a variable to its list of bound variables " +
    "provided that the variable is not already bound" in {
    val abstTerm = Abst(varTermX, varTermY)
    abstTerm.free should be(Set(varNameX))
    abstTerm.bound should be(Set(varNameY))
  }

  "An abstraction over a free variable already in a term" should " add a variable to its list of bound variables " in {
    val abstTerm1 = Abst(varTermX, varTermY)
    val abstTerm2 = Abst(abstTerm1, varTermX)
    abstTerm2.free should be(Set.empty)
    abstTerm2.bound should be(Set(varNameY, varNameX))
  }

  "An abstraction over an application " should " add a variable to its list of bound variables " +
    "provided that the variable is not already bound" in {
    val appTerm = App(varTermX, varTermY)
    appTerm.free should be(Set(varNameX, varNameY))
    appTerm.bound should be(Set.empty)
  }

  "An abstraction over an abstraction " should " add a variable to its list of bound variables " +
    "provided that the variable is not already bound" in {

    //Create an abstraction term to test. Variable  "abstVarIn" should be immediately added to the list of bound variables.
    val abstTermIn = Abst(varTermX, varTermY)

    //Check that the variable being abstracted over is added to the list of bound variables but the variable for the original term is not
    // Creating local function to test same conditions as postconditions of abstracting over an abstraction term


    def yShouldBeBoundInOriginal = abstTermIn.bound should be(Set(varNameY))

    def xShouldBeFreeInOriginal = abstTermIn.free should be(Set(varNameX))

    yShouldBeBoundInOriginal
    xShouldBeFreeInOriginal


    //Now abstract over the abstraction term
    val abstVarNameOut = varNameX + "1" //Guarantee this is a new variable
    val abstVarOut = Var(abstVarNameOut)
    val abstTermOut = Abst(abstTermIn, abstVarOut)
    abstTermOut.free should be(Set(varNameX))
    abstTermOut.bound should be(Set(varNameY, abstVarNameOut))

    //Ensure original term is not affected
    yShouldBeBoundInOriginal
    xShouldBeFreeInOriginal

  }

  "An abstraction over a bound variable" should "throw an exception" in {
      intercept[RuntimeException](Abst(Abst(Var("Y"), Var("X")), Var("X")))
    }

}
