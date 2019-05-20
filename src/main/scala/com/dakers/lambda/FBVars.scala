package com.dakers.lambda

import com.dakers.lambda.lambda2.*

trait FBVars {

  def free(): Set[*]

  def bound(): Set[*]

}
