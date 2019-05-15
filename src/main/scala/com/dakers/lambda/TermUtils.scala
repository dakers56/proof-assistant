package com.dakers.lambda

object TermUtils {

  def intersection[T <: UTTerm](t1: T, t2: T) = (t1.free union t1.bound) intersect (t2.free union t2.bound)

  def checkVars[T <: UTTerm](t1: T, t2: T) = {
    val commonVars = intersection(t1, t2)
    if (!commonVars.isEmpty) throw new RuntimeException(s"Terms $t1 and $t2 had variable(s) in common")
  }
}
