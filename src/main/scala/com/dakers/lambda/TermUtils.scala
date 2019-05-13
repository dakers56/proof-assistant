package com.dakers.lambda

object TermUtils {

  def intersection(t1: UTTerm, t2: UTTerm) = (t1.free union t1.bound) intersect (t2.free union t2.bound)

  def checkVars(t1: UTTerm, t2: UTTerm) = {
    val commonVars = intersection(t1, t2)
    if (!commonVars.isEmpty) throw new RuntimeException(s"Terms $t1 and $t2 had variable(s) in common")
  }
}
