package com.dakers

package object lambda {

  val AbstOp = raw"/|"
  val AbstSep = "."
  val AbstRegex = raw"/\|(.)\.(.*)".r
  val VarRegex = "(.)".r
  val AppLRegex = raw"\((\(.*\))(.)\)".r
  val AppRRegex = raw"\((.)(\(.*\))\)".r
  val App0Regex = raw"\((.)(.)\)".r
//  val AppAbstR = raw"(.*)(?=\/\|)(.*)(?=\))".r
  val AppAbstR = raw"\((.*)/\|(.)\.(.*)\)".r


  def lam(s: String): Term = {

    s match {
      case AbstRegex(x, t) => {
        Abst(lam(t), Var(x))
      }
      case AppLRegex(l, r) => {
        App(lam(l), lam(r))
      }
      case AppRRegex(l, r) => {
        App(lam(l), lam(r))
      }
      case App0Regex(l, r) => {
        App(Var(l), Var(r))
      }
      case VarRegex(x) => {
        Var(x)
      }
      case AppAbstR(l,x, r) => {
        App(lam(l), Abst(lam(r), Var(x)))
      }
      case _ => {
        throw new RuntimeException(s"$s was not a well formed lambda expression. Make sure that it has () around all application terms.")
      }
    }
  }


}