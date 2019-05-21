package com.dakers.lambda.proof

import com.dakers.lambda.lambda2.L2Notation
import com.dakers.lambda.{Abst, STNotation, UTNotation}
import org.scalatest.{FlatSpec, Matchers}

/**
 * This class is designed to demonstrate proofs in the second order lambda calculus using the derivation system
 * created in this project.
 */
class Demo extends FlatSpec with Matchers with STNotation with UTNotation with L2Notation {

  /**
   * We will begin by proving some simple facts of logic.
   * Firstly note that we can encode true and false as follows.
   */

  val True: Abst = /|("x", /|("y", "x"))
  val False: Abst = /|("u", /|("v", "v"))

  /**
   * This wouldn't be very interesting unless we could do something with it though.
   * To that end, let's define negation in the untyped lambda calculus.
   */

  def Not: Abst = /|("a", /|("x", /|("y", ("a" * "y") * "x")))


  //  "The negation of true" should "be false" in {
  //    val s0 = Not * False
  //    val s1 = BetaRed.red1s(s0, "a")
  //    var s2 = AlphaConv.Mxy(s1, "x", "z")
  //    s2 = BetaRed.red1s(s1, "u") ///|x./|y.((/|u./|v.vy)x)
  //    val s3 = BetaRed.red1s(s2, "y")
  //    val s4 = BetaRed.red1s(s3, "z")
  //    val s5 = BetaRed.red1s(s4, "u")
  //    s5 should be(False)
  //  }

}
