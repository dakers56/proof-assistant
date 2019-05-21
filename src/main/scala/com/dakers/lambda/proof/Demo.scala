package com.dakers.lambda.proof

import com.dakers.lambda.lambda2.L2Notation
import com.dakers.lambda.{Abst, STNotation, UTNotation}

/**
 * This class is designed to demonstrate proofs in the second order lambda calculus using the derivation system
 * created in this project.
 */
object Demo extends  STNotation with UTNotation with L2Notation {

  /**
   * We will begin by proving some simple facts of logic.
   * Firstly note that we can encode true and false as follows.
   */

  val True: Abst = /|("x", /|("y", "x"))
  val False: Abst = /|("y", /|("x", "x"))

  /**
   * This wouldn't be very interesting unless we could do something with it though.
   * To that end, let's define negation in the untyped lambda calculus.
   */




}
