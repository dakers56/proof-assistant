package com.dakers.lambda

case class STTerm[T <: SimpleType](val term: UTTerm, val sType: T) {
  val TypeSep = ":"

  override def toString: String = s"$term $TypeSep $sType"

  def typer(vType: VarType) = STTerm(Var("c"), VarType("b"))

  def `:`(varType: VarType) = typer(varType)

  def ofType(varType: VarType) = typer(varType)

  def main(args: Array[String]): Unit = {
    val test = STTerm(Var("x"), VarType("x"))
    test ofType VarType("x")
  }


}
