package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.CodeGen.CodeGenContext

/**
 * User: mthorpe
 * Date: 22/06/2013
 * Time: 20:38
 */
sealed abstract class TMode(implicit val scope: Scope)              extends Typer {
  override def codegen(implicit indent: Int, context: CodeGenContext): String = ???
}

final case class TReadOnly(implicit override val scope: Scope)      extends TMode {
  override def toString = "TReadOnly"
}

final case class TImmutable(implicit override val scope: Scope)     extends TMode {
  override def toString = "TImmutable"
}

final case class TMutable(implicit override val scope: Scope)       extends TMode {
  override def toString = "TMutable"
}

final case class TUnique(implicit override val scope: Scope)        extends TMode {
  override def toString = "TUnique"
}

final case class TModeExpr(expr: TExpr)(implicit override val scope: Scope) extends TMode {
  override def toString = s"TModeExpr(expr = $expr)"
}
