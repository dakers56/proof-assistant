package com.dakers.lambda.lambda2

import com.dakers.lambda.{DerivationContext, STNotation}

class L2StatementContext extends DerivationContext[L2Statement] {
  def free(): Set[String] = stmts().foldLeft(scala.collection.mutable.Set[String]())((acc, i) => acc ++ i.utTerm.free).toSet

  def bound(): Set[String] = stmts().foldLeft(scala.collection.mutable.Set[String]())((acc, i) => acc ++ i.utTerm.bound).toSet

  override def proj(vars: Set[String]): List[L2Statement] = stmts().filter(s => s.utTerm.free subsetOf (free()))

  override def stmts(): List[L2Statement] = super.stmts()
}

object L2StatementContext {
  def apply(l: List[L2Statement]): L2StatementContext = {
    val n = new L2StatementContext()
    l.foreach(i => n.add(i))
    n
  }
}

class L2TypeDeclContext extends DerivationContext[L2TypeDecl] {
  def free(): Set[String] = stmts().foldLeft(scala.collection.mutable.Set[String]())((acc, i) => acc += i.nm).toSet

  def bound(): Set[String] = stmts().foldLeft(scala.collection.mutable.Set[String]())((acc, i) => acc += i.nm).toSet

  override def proj(vars: Set[String]): List[L2TypeDecl] = stmts().filter(s => free().contains(s.nm))

  override def stmts(): List[L2TypeDecl] = super.stmts()
}

object L2TypeDeclContext {
  def apply(l: List[L2TypeDecl]): L2TypeDeclContext = {
    val n = new L2TypeDeclContext()
    l.foreach(i => n.add(i))
    n
  }
}

case class L2Context(val stmtContext: L2StatementContext, val typeContext: L2TypeDeclContext) {


  def add(t: L2Statement): Unit = {
    if (!(t.utTerm.free intersect stmtContext.bound()).isEmpty)
      throw new RuntimeException("Cannot add a term to a context if one of its free terms is already bound. Term free vars: " + t.utTerm.free + "; context free vars: " + free())
    stmtContext.add(t)
  }

  def add(t: L2TypeDecl): Unit = {
    if (freeT().contains(t.nm))
      throw new RuntimeException("Cannot add a term to a context if one of its free terms is already free in the context. Term free vars: " + t.nm + "; context free vars: " + freeT())
    if (boundT().contains(t.nm))
      throw new RuntimeException("Cannot add a term to a context if one of its free terms is already bound. Term free vars: " + t.nm + "; context free vars: " + boundT())
    typeContext.add(t)
  }

  def freeT(): Set[String] = typeContext.free()

  def boundT(): Set[String] = typeContext.bound()

  def free(): Set[String] = stmtContext.free()

  def bound(): Set[String] = stmtContext.bound()

  def del(l2TypeDecl: L2TypeDecl): Unit = typeContext.del(l2TypeDecl)

  def del(l2Statement: L2Statement): Unit = stmtContext.del(l2Statement)

  def dom(): Set[String] = typeContext.dom() union stmtContext.dom()

}

object L2Context extends STNotation {
  def apply(l1: List[L2Statement], l2: List[L2TypeDecl]): L2Context = new L2Context(L2StatementContext(l1), L2TypeDeclContext(l2))

  def apply(l1: List[L2Statement]): L2Context = new L2Context(L2StatementContext(l1), L2TypeDeclContext(List()))

  def apply(): L2Context = new L2Context(L2StatementContext(Nil), L2TypeDeclContext(Nil))

}
