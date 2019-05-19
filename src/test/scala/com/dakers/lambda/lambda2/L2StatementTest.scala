package com.dakers.lambda.lambda2

import com.dakers.lambda.stlc.SimpleType
import com.dakers.lambda.{L2Conv, STNotation, UTNotation, UTTerm}
import org.scalatest.{FlatSpec, Matchers}

class L2TermTest extends FlatSpec with Matchers with STNotation with UTNotation with L2Conv {

  "A simply typed variable term" should "create a valid second order lambda term" in {
    val st: SimpleType = "X"
    val term: UTTerm = "x"
    val l2Term = L2Term(term, st)
    l2Term.l2Type should be(L2SimpleType(st))
    l2Term.term should be(term)
  }

  "A simply typed application term" should "create a valid second order lambda term" in {
    val st: SimpleType = "X"
    val term: UTTerm = "x"
    val l2Term = L2Term(term, st)
    l2Term.l2Type should be(L2SimpleType(st))
    l2Term.term should be(term)
  }

  "A simply typed abstraction term" should "create a valid second order lambda term" in {
    val st: SimpleType = "X"
    val term: UTTerm = /|("x", "X")
    val l2Term = L2Term(term, st)
    l2Term.l2Type should be(L2SimpleType(st))
    l2Term.term should be(term)
  }

  "A simply typed application including an abstraction term" should "create a valid second order lambda term" in {
    val st: SimpleType = "X"
    val term: UTTerm = "x" * /|("y", "Y")
    val l2Term = L2Term(term, st)
    l2Term.l2Type should be(L2SimpleType(st))
    l2Term.term should be(term)
  }

  "A simply typed application of two abstraction terms" should "create a valid second order lambda term" in {
    val st: SimpleType = "X"
    val term: UTTerm = /|("w", "W") * /|("y", "Y")
    val l2Term = L2Term(term, st)
    l2Term.l2Type should be(L2SimpleType(st))
    l2Term.term should be(term)
  }

  "An L2Term consisting of a single variable with a pi type" should "create a valid second order lambda term" in {
    val st: π = π("x", "X")
    val term: UTTerm = "x"
    val l2Term = L2Term(term, st)
    l2Term.l2Type should be(L2PiType(st))
    l2Term.term should be(term)
  }

  "An L2Term consisting of an application term and a pi type" should "create a valid second order lambda term" in {
    val st: π = π("x", "X")
    val term: UTTerm = /|("w", "W") * /|("y", "Y")
    val l2Term = L2Term(term, st)
    l2Term.l2Type should be(L2PiType(st))
    l2Term.term should be(term)
  }

  "An L2Term consisting of an abstraction term with a pi type" should "create a valid second order lambda term" in {
    val st: π = π("x", "X")
    val term: UTTerm = /|("w", "W")
    val l2Term = L2Term(term, st)
    l2Term.l2Type should be(L2PiType(st))
    l2Term.term should be(term)
  }
}
