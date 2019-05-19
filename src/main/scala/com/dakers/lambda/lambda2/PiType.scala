package com.dakers.lambda.lambda2

import com.dakers.lambda.stlc.{STStatement, SimpleType}

/**
 *
 * @param term    Same as an untyped term, but the terms are to be interpreted as types.
 * @param depType The typed bound by the π binder
 */
case class π(term: STStatement, depType: SimpleType) {
  override def toString: String = s"π($depType:*.$term)"
}
