package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.AST.UnaryOp
import com.github.mt2309.pony.CodeGen.CodeGenContext

/**
 * User: mthorpe
 * Date: 23/06/2013
 * Time: 22:50
 */
sealed abstract class TUnary(implicit val scope: Scope) extends Typer {
  def unaryOps: List[UnaryOp]
  def extractOfType: Option[TOfType]
  def isSimple: Boolean
  def tail(implicit indent: Int, context: CodeGenContext): String
  override def codegen(implicit indent: Int, context: CodeGenContext): String
}

final case class TUnaryCommand(unaryOps: List[UnaryOp], command: TCommand)(implicit override val scope: Scope) extends TUnary {
  override def extractOfType = command.extractOfType

  override def isSimple: Boolean = command.isSimple

  override def codegen(implicit indent: Int, context: CodeGenContext): String = {
    val b = new StringBuilder

    for (op <- unaryOps)
      b.append(op.codeGen)

    b.append(command.codegen)

    b.mkString
  }

  override def tail(implicit indent: Int, context: CodeGenContext): String = command.tail

  override def toString = s"TUnaryCommand(unaryOps = $unaryOps, command = $command)"
}

final case class TUnaryLambda(unaryOps: List[UnaryOp], lambda: TLambda)(implicit override val scope: Scope) extends TUnary {
  lazy val extractOfType = Some(new TOfType(Set(lambda)))

  override def codegen(implicit indent: Int, context: CodeGenContext): String = ???

  override def tail(implicit indent: Int, context: CodeGenContext): String = throw new UnsupportedOperationException

  val isSimple: Boolean = false
}
