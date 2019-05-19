package com.dakers.lambda

import org.scalatest.{FlatSpec, Matchers}

class PiTypeTest extends FlatSpec with Matchers with UTNotation with STNotation {

  "A pi type over a type variable" should "have a toString() representation of π($v:*.$v)" in {
    for (v <- 'a' to 'z') {
      val expected = s"π($v:*.$v)"
      π(Var(v.toString), VarType(v.toString)).toString should be(expected)
    }
  }

  "A pi type " should "be able to quantify over arrow types and simple types" in {
    var st =  π("x", "V")
    st.term should be(Var("x"))
    st.depType should be(VarType("V"))

    val arr : ArrType= ArrType("X", "Y")
    val term = /|("x", "y")
    st =  π(term, arr)
    st.term should be(term)
    st.depType should be(arr)


  }

}
