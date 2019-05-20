package com.dakers.lambda.lambda2

import com.dakers.lambda.{STNotation, UTNotation, stlc}
import com.dakers.lambda.stlc.{ArrType, STStatement, VarType}
import org.scalatest.{FlatSpec, Matchers}

class L2StatementTest extends FlatSpec with Matchers with UTNotation with STNotation {

  "A 2nd order term with a variable type " should " have a variable as its type" in {
    val term = "x" * "y"
    val varType = VarType("X")
    val stTerm = L2Statement(term, varType)
    stTerm.utTerm should be(term)
    stTerm.l2Type should be(varType)
  }

  "The string representation of a term with a variable type " should " be the term followed by a separator followed by its type" in {
    val term = "x" * "y"
    val varType = VarType("X")
    val stTerm = L2Statement(term, varType)
    stTerm.toString should be(term + ":" + varType)
  }

  "A term with an arrow type " should " have an arrow type" in {
    val term = "x" * "y"
    val varType1 = VarType("X")
    val varType2 = VarType("Y")
    val arrowType = ArrType2(varType1, varType2)

    val stmt = L2Statement(term, arrowType)
    stmt.utTerm should be(term)
    stmt.l2Type should be(ArrType2(varType1, varType2))
  }

  "The string representation of a term with an arrow type " should " be the term followed by a separator followed by an arrow type" in {
    val term = "x" * "y"
    val varType1 = VarType("X")
    val varType2 = VarType("Y")
    val arrowType = ArrType2(varType1, varType2)
    val stTerm = L2Statement(term, arrowType)
    stTerm.toString should be(term + ":" + arrowType)
  }

  "Any term " should " be able to be from a variable to an arrow type, and vice versa" in {
    val term = "x" * "y"
    val varType1 = VarType("X")
    val varType2 = VarType("Y")
    val arrowType1 = ArrType2(varType1, varType2)
    val arrowType2 = ArrType2(varType1, arrowType1)

    val stTerm = L2Statement(term, arrowType2)
    stTerm.utTerm should be(term)
    stTerm.l2Type should be(ArrType2(varType1, arrowType1))

    val arrowType3 = ArrType2(arrowType1, varType1)
    val stTerm1 = L2Statement(term, arrowType3)
    stTerm1.utTerm should be(term)
    stTerm1.l2Type should be(arrowType3)
  }


}
