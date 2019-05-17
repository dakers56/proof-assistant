package com.dakers.lambda


abstract class DerivationContext[T](private var _stmts: List[T] = List()) {

  def stmts(): List[T] = _stmts

  def varCount(v: String): Int

  def add(t: T): Unit = {
    _stmts.contains(t) match {
      case false => _stmts = _stmts :+ t
      case true => _stmts = _stmts
    }
  }

  def del(t: T): Unit = {
    _stmts = _stmts.filter(u => u != t)
  }

  override def toString: String = _stmts.map(x => x.toString).mkString(",")

  def canEqual(other: Any): Boolean = other.isInstanceOf[DerivationContext[T]]

  override def equals(other: Any): Boolean = other match {
    case that: DerivationContext[T] =>
      (that canEqual this) &&
        _stmts == that._stmts
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(_stmts)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

class UntypedDerivationContext extends DerivationContext[UTTerm] {
  override def varCount(v: String): Int = super.stmts().map(t => t.varNames.contains(v)).count(u => u)
}

object UntypedDerivationContext {
  def apply(): UntypedDerivationContext = new UntypedDerivationContext()
}


class SimplyTypedDerivationContext extends DerivationContext[Statement] {
  override def varCount(v: String): Int = super.stmts().map(t => t.term).map(t => t.varNames.contains(v)).count(u => u)
}

object SimplyTypedDerivationContext extends STNotation {
  def apply(): SimplyTypedDerivationContext = new SimplyTypedDerivationContext()

  def apply(stmts: List[Statement]): SimplyTypedDerivationContext = {
    val ctx = SimplyTypedDerivationContext()
    stmts.foreach(t => ctx.add(t))
    ctx
  }
}