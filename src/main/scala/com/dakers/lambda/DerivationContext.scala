package com.dakers.lambda


abstract class DerivationContext[T](private var _context: List[T] = List()) {

  def stmts(): List[T] = _context

  def varCount(v: String): Int

  def add(t: T): Unit = {
    _context.contains(t) match {
      case false => _context = t :: _context
      case true => _context = _context
    }
  }

  def del(t: T): Unit = {
    _context = _context.filter(u => u != t)
  }

}

class UntypedDerivationContext extends DerivationContext[UTTerm] {
  override def varCount(v: String): Int = super.stmts().map(t => t.varNames.contains(v)).count(u => u)
}

object UntypedDerivationContext {
  def apply(): UntypedDerivationContext = new UntypedDerivationContext()
}


class SimplyTypedDerivationContext extends DerivationContext[STTerm] {
  override def varCount(v: String): Int = super.stmts().map(t => t.term).map(t => t.varNames.contains(v)).count(u => u)
}

object SimplyTypedDerivationContext {
  def apply(): SimplyTypedDerivationContext = new SimplyTypedDerivationContext()
}