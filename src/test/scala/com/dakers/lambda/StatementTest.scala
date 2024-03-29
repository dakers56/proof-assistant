package com.dakers.lambda

import com.dakers.lambda.stlc.{ArrType, STStatement, VarType}
import org.scalatest.{FlatSpec, Matchers}

class StatementTest extends FlatSpec with Matchers with STNotation with UTNotation {

  "A term with a variable type " should " have a variable as its type" in {
    val term = "x" * "y"
    val varType = VarType("X")
    val stTerm = STStatement(term, varType)
    stTerm.term should be(term)
    stTerm.sType should be(varType)
  }

  "The string representation of a term with a variable type " should " be the term followed by a separator followed by its type" in {
    val term = "x" * "y"
    val varType = VarType("X")
    val stTerm = stlc.STStatement(term, varType)
    stTerm.toString should be(term + ":" + varType)
  }

  "A term with an arrow type " should " have an arrow type" in {
    val term = "x" * "y"
    val varType1 = VarType("X")
    val varType2 = VarType("Y")
    val arrowType = ArrType(varType1, varType2)

    val stTerm = stlc.STStatement(term, arrowType)
    stTerm.term should be(term)
    stTerm.sType should be(ArrType(varType1, varType2))
  }

  "The string representation of a term with an arrow type " should " be the term followed by a separator followed by an arrow type" in {
    val term = "x" * "y"
    val varType1 = VarType("X")
    val varType2 = VarType("Y")
    val arrowType = ArrType(varType1, varType2)
    val stTerm = stlc.STStatement(term, arrowType)
    stTerm.toString should be(term + ":" + arrowType)
  }

  "Any term " should " be able to be from a variable to an arrow type, and vice versa" in {
    val term = "x" * "y"
    val varType1 = VarType("X")
    val varType2 = VarType("Y")
    val arrowType1 = ArrType(varType1, varType2)
    val arrowType2 = ArrType(varType1, arrowType1)

    val stTerm = stlc.STStatement(term, arrowType2)
    stTerm.term should be(term)
    stTerm.sType should be(ArrType(varType1, arrowType1))

    val arrowType3 = ArrType(arrowType1, varType1)
    val stTerm1 = stlc.STStatement(term, arrowType3)
    stTerm1.term should be(term)
    stTerm1.sType should be(arrowType3)
  }

}
