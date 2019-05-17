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

  def free(): Set[String]

  def bound(): Set[String]

  def dom(): Set[String] = free union bound

  def proj(vars: Set[String]): Set[String] = vars intersect dom()

  def isPerm(d: DerivationContext[T]): Boolean = {
    stmts().foreach(s => if (!d.stmts().contains(s)) return false)
    d.stmts().foreach(s => if (!stmts().contains(s)) return false)
    return true
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

  override def add(t: UTTerm): Unit = {
    if (!(t.free intersect bound()).isEmpty)
      throw new RuntimeException("Cannot add a term to a context if one of its free terms is already bound. Term free vars: " + t.free + "; context free vars: " + free())
    if (!(t.bound intersect free).isEmpty)
      throw new RuntimeException("Cannot add a term to a context if one of its free terms is already bound. Term free vars: " + t.free + "; context free vars: " + free)
    super.add(t)
  }

  override def free(): Set[String] = stmts().foldLeft(scala.collection.mutable.Set[String]())((acc, i) => acc ++ i.free).toSet

  override def bound(): Set[String] = stmts().foldLeft(scala.collection.mutable.Set[String]())((acc, i) => acc ++ i.bound).toSet
}

object UntypedDerivationContext {
  def apply(): UntypedDerivationContext = new UntypedDerivationContext()
}


class SimplyTypedDerivationContext extends DerivationContext[Statement] {
  override def varCount(v: String): Int = super.stmts().map(t => t.term).map(t => t.varNames.contains(v)).count(u => u)

  override def free(): Set[String] = stmts().foldLeft(scala.collection.mutable.Set[String]())((acc, i) => acc ++ i.term.free).toSet

  override def bound(): Set[String] = stmts().foldLeft(scala.collection.mutable.Set[String]())((acc, i) => acc ++ i.term.bound).toSet

  override def add(t: Statement): Unit = {
    if (!(t.term.free intersect bound()).isEmpty)
      throw new RuntimeException("Cannot add a term to a context if one of its free terms is already bound. Term free vars: " + t.term.free + "; context free vars: " + free())
    if (!(t.term.bound intersect free).isEmpty)
      throw new RuntimeException("Cannot add a term to a context if one of its free terms is already bound. Term free vars: " + t.term.free + "; context free vars: " + free)
    super.add(t)
  }
}

object SimplyTypedDerivationContext extends STNotation {
  def apply(): SimplyTypedDerivationContext = new SimplyTypedDerivationContext()

  def apply(stmts: List[Statement]): SimplyTypedDerivationContext = {
    val ctx = SimplyTypedDerivationContext()
    stmts.foreach(t => ctx.add(t))
    ctx
  }
}

