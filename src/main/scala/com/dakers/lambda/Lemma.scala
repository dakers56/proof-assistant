package com.dakers.lambda

object Lemma {

  def isSubcontext(g1: List[Statement], g2: List[Statement]): Boolean = {
    g1 match {
      case Nil => true
      case h :: t => g2 match {
        case Nil => false
        case h1 :: t1 => if (h == h1) isSubcontext(t, t1) else false
      }
    }
  }

}
