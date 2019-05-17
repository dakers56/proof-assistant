package com.dakers.lambda

import com.dakers.lambda.Subcontext.isSubcontext
import org.scalatest.{FlatSpec, Matchers}

class SubcontextTest extends FlatSpec with Matchers with UTNotation with STNotation {

  //positive cases
  "The empty context" should "be a subcontext of the empty contest" in {
    isSubcontext(Nil, Nil) should be(true)
  }

  "The empty context" should "be a subcontext of a non-empty contest" in {
    isSubcontext(Nil, List("x" :| "X")) should be(true)
    isSubcontext(Nil, List("x" :| "X", "y" :| "Y")) should be(true)
    isSubcontext(Nil, List("x" :| "X", "y" :| "Y", "z" :| "Z")) should be(true)
  }

  "A context " should "be a subcontext of itself" in {
    isSubcontext(List("x" :| "X"), List("x" :| "X")) should be(true)
    isSubcontext(List("x" :| "X", "y" :| "Y"), List("x" :| "X", "y" :| "Y")) should be(true)
    isSubcontext(List("x" :| "X", "y" :| "Y", "z" :| "Z"), List("x" :| "X", "y" :| "Y", "z" :| "Z")) should be(true)
  }

  "A context such that all of its elements are in another" should "be a subcontext of the other" in {
    isSubcontext(List("x" :| "X"), List("x" :| "X")) should be(true)
    isSubcontext(List("x" :| "X"), List("x" :| "X", "y" :| "Y")) should be(true)
    isSubcontext(List("x" :| "X", "y" :| "Y"), List("x" :| "X", "y" :| "Y", "z" :| "Z")) should be(true)
  }

  //negative cases
  "A non-empty context " should "not be a subcontext of the empty context" in {
    isSubcontext(List("x" :| "X"), List()) should be(false)
    isSubcontext(List("x" :| "X"), List()) should be(false)
    isSubcontext(List("x" :| "X", "y" :| "Y"), List()) should be(false)
  }

  "A context containing at least one element not in another" should "be a subcontext of the other" in {
    isSubcontext(List("x" :| "X", "a" :| "A"), List("x" :| "X")) should be(false)
    isSubcontext(List("x" :| "X", "a" :| "A"), List("x" :| "X", "y" :| "Y")) should be(false)
    isSubcontext(List("x" :| "X", "y" :| "Y", "a" :| "A"), List("x" :| "X", "y" :| "Y", "z" :| "Z")) should be(false)
  }

}
