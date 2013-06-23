package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.CodeGen.CodeGenContext

/**
 * User: mthorpe
 * Date: 22/06/2013
 * Time: 20:38
 */
sealed abstract class TMode(implicit val scope: Scope)              extends Typer {
  override def codegen(implicit indent: Int, context: CodeGenContext): String = ???

  def isSubType(that: TMode): Boolean
}

final case class TReadOnly(implicit override val scope: Scope)      extends TMode {
  override def toString = "TReadOnly"

  override def isSubType(that: TMode): Boolean = that match {
    case r: TReadOnly => true
    case i: TImmutable => false
    case m: TMutable => false
    case u: TUnique => false
    case TModeExpr(expr) => ???
  }
}

final case class TImmutable(implicit override val scope: Scope)     extends TMode {
  override def toString = "TImmutable"
  override def isSubType(that: TMode): Boolean = that match {
    case r: TReadOnly => true
    case i: TImmutable => true
    case m: TMutable => false
    case u: TUnique => false
    case TModeExpr(expr) => ???
  }
}

final case class TMutable(implicit override val scope: Scope)       extends TMode {
  override def toString = "TMutable"
  override def isSubType(that: TMode): Boolean = that match {
    case r: TReadOnly => true
    case i: TImmutable => false
    case m: TMutable => true
    case u: TUnique => false
    case TModeExpr(expr) => ???
  }
}

final case class TUnique(implicit override val scope: Scope)        extends TMode {
  override def toString = "TUnique"
  override def isSubType(that: TMode): Boolean = that match {
    case r: TReadOnly => true
    case i: TImmutable => true
    case m: TMutable => true
    case u: TUnique => true
    case TModeExpr(expr) => ???
  }
}

final case class TModeExpr(expr: TExpr)(implicit override val scope: Scope) extends TMode {
  override def toString = s"TModeExpr(expr = $expr)"
  override def isSubType(that: TMode): Boolean = that match {
    case r: TReadOnly => ???
    case i: TImmutable => ???
    case m: TMutable => ???
    case u: TUnique => ???
    case TModeExpr(e) => ???
  }
}
