package com.dakers.lambda

import org.scalatest.{FlatSpec, Matchers}

class PackageTest extends FlatSpec with Matchers {


  "x.decl()" should "create Var(x), add Var(x) to the context, and x to the list of variable names" in {
    "x".decl()
    context should be(scala.collection.mutable.Map(("x" -> Var("x"))))
    varNames should be(scala.collection.mutable.ListBuffer("x"))
    context.clear()
    varNames.clear()
  }

  "x.decl(), then y.decl()" should "create Var(x), add Var(x) to the context, and x to the list of variable names, and similarly for y" in {
    "x".decl()
    "y".decl()
    context should be(scala.collection.mutable.Map(("x" -> Var("x")), ("y" -> Var("y"))))
    varNames should be(scala.collection.mutable.ListBuffer("x", "y"))

    context.clear()
    varNames.clear()
  }

  "If x is already in the context, x.decl()" should " throw an exception" in {
    intercept[RuntimeException]({
      "x".decl();
      "x".decl()
    })
    context.clear()
    varNames.clear()
  }

  "The ? operator " should " be able to get a term which is already in the context" in {
    "x".decl()
    "x".? should be(Var("x"))
    "y".decl()
    "y".? should be(Var("y"))

    val appTerm = "x".? * "y".?
    context += (appTerm.toString -> appTerm)
    appTerm.toString.? should be(appTerm)
    context -= appTerm.toString

    val abstTerm = /|("x", "y".?)
    context += (abstTerm.toString -> abstTerm)
    abstTerm.toString.? should be(abstTerm)

    context.clear()
    varNames.clear()
  }

  "The ? operator " should " throw an exception when a term is not yet in the context" in {

    context.clear()
    varNames.clear()

    intercept[RuntimeException]({
      "x".?
    })

    intercept[RuntimeException]({
      "x".decl();
      "y".decl();
      (Var("x") * Var("y")).toString.?
    })
    context.clear()
    varNames.clear()

    intercept[RuntimeException]({
      "x".decl();
      "y".decl();
      /|("x", "y".?).toString.?
    })
    context.clear()
    varNames.clear()


  }

  "+~ " should " add a term to the context" in {
    context.clear()
    varNames.clear()
    +~("x".decl())
    context should be(scala.collection.mutable.Map(("x" -> Var("x"))))
    +~("y".decl())
    context should be(scala.collection.mutable.Map(("x" -> Var("x")), ("y" -> Var("y"))))
    context.clear()
    varNames.clear()

    +~("x".decl() * "y".decl())
    val appTerm = App(Var("x"), Var("y"))
    context should be(scala.collection.mutable.Map(("x" -> Var("x")), ("y" -> Var("y")), (appTerm.toString -> appTerm)))
    context.clear()
    varNames.clear()

    "x".decl()
    val abstTerm = /|("x", "x".?)
    +~(abstTerm)
    context should be(scala.collection.mutable.Map(("x" -> Var("x")), (abstTerm.toString -> abstTerm)))
    context.clear()
    varNames.clear()

  }

  "-~ " should " remove a term from the context" in {
    context.clear()
    varNames.clear()

    +~("x".decl())
    +~("y".decl())

    -~(Var("y"))
    context should be(scala.collection.mutable.Map(("x" -> Var("x"))))

    -~(Var("x"))
    context should be(scala.collection.mutable.Map())

    context.clear()
    varNames.clear()

    val appTerm = "x".decl() * "y".decl()
    +~(appTerm)
    -~(appTerm)
    context should not contain (appTerm)

    val abstTerm = /|("x", "x".?)
    +~(abstTerm)
    -~(abstTerm)
    context should not contain (abstTerm)

    context.clear()
    varNames.clear()

  }

  "An application term " should "be created if its constituents are declared." in {
    "x".decl() * "y".decl() should be(App(Var("x"), Var("y")))
    context.clear()
    varNames.clear()
  }

  "An application term " should "not be created if its constituents are not declared." in {

    intercept[RuntimeException](Var("x") * Var("y"))

    intercept[RuntimeException]("x".decl() * Var("y"))
    context.clear()
    varNames.clear()

    intercept[RuntimeException](Var("x") * "y".decl())
    context.clear()
    varNames.clear()

  }

  "An abstraction term " should "be created if its constituents are declared." in {
    "x".decl()
    /|("x", "y".decl()) should be(Abst(Var("y"), Var("x")))
    context.clear()
    varNames.clear()
  }

  "An abstraction term " should "not be created if its constituents are not declared." in {

    intercept[RuntimeException](/|("x", Var("y")))

    intercept[RuntimeException]({
      "x".decl();
      /|("x", Var("y"))
    })
    context.clear()
    varNames.clear()

    intercept[RuntimeException](/|("x", "y".decl()))
    context.clear()
    varNames.clear()

  }

}
