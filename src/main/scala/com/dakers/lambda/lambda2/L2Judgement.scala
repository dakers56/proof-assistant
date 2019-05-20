package com.dakers.lambda.lambda2

case class L2Judgement(l2Context: L2Context, s: Option[L2Statement], t: Option[L2TypeDecl]) {
  override def toString() = l2Context.toString + "|-" + s.getOrElse(t.get).toString
}

object L2Judgement {
  def apply(l2Context: L2Context, s: L2Statement): L2Judgement = new L2Judgement(l2Context, Some(s), None)

  def apply(l2Context: L2Context, s: L2TypeDecl): L2Judgement = new L2Judgement(l2Context, None, Some(s))
}
