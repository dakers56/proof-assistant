package com.dakers

package object lambda {

  val AbstOp = raw"/|"
  val AbstSep = raw"."
  val AbstRegex = s"^$AbstOp(.*)".r
  val VarRegex = raw"(.)".r //For now, only handling 1-character variables
  val AppRegex = raw"(.{2,})".r


  def lam(s: String): Term = {
    if (s.length == 1) return Var(s)
    if (s.startsWith(AbstOp)) {
      val v = splitAbst(s)
      Abst(lam(v._2), Var(v._1))
    }
    else {
      val v = splitApp(s)
      if (v._2.length == 1) {
        App(Var(v._1), lam(v._2))
      }
      else {
        val x = v._1
        val y = v._2.charAt(0).toString
        val xy = App(Var(x), Var(y))
        var xyz = xy

        for (i <- 1 until v._2.length) {
          xyz = App(xyz, Var(v._2.charAt(i).toString))
        }
        xyz
      }
    }
  }


  def splitApp(s: String) = {
    (s.substring(0, 1), s.substring(1))
  }

  def splitAbst(s: String): (String, String) = {
    val i = s.indexOf(AbstSep)
    (s.substring(0, i).replace(AbstOp, ""), s.substring(i + 1))
  }

  def varName(binderStr: String) = binderStr.replaceFirst(AbstOp, "")


}