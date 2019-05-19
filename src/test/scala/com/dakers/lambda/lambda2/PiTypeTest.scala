package com.dakers.lambda.lambda2

import com.dakers.lambda.stlc.{ArrType, VarType}
import com.dakers.lambda.{STNotation, UTNotation, lambda2}
import org.scalatest.{FlatSpec, Matchers}

class PiTypeTest extends FlatSpec with Matchers with UTNotation with STNotation {

  "A pi type over a type variable" should "have a toString() representation of π($v:*.$v)" in {
    for (v <- 'a' to 'z') {
      val expected = s"π($v:*.$v:X)"
      π(v.toString :| "X", VarType(v.toString)).toString should be(expected)
    }
  }

  "A pi type " should "be able to quantify over arrow types and simple types" in {
    val st = "x" :| "X"
    var l2Term = lambda2.π(st, "X")
    l2Term.term should be(st)
    l2Term.depType should be(VarType("X"))

    val arr: ArrType = ArrType("X", "Y")
    val term = /|("x", "y") :| arr
    l2Term = lambda2.π(term, arr)
    l2Term.term should be(term)
    l2Term.depType should be(arr)


  }

}
