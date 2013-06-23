package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.CodeGen.CodeGenContext

/**
 * User: mthorpe
 * Date: 22/06/2013
 * Time: 20:21
 */
sealed abstract class TSecondCommand(implicit val scope: Scope) extends Typer {
  def extractOfType(fst: Option[TOfType]): Option[TOfType]
  def tail(implicit indent: Int, context: CodeGenContext): String
  override def codegen(implicit indent: Int, context: CodeGenContext): String
}

final case class TSecondCommandArgs(args: TArgs)(implicit override val scope: Scope) extends TSecondCommand with Typer {
  override def extractOfType(fst: Option[TOfType]): Option[TOfType] = fst

  override def codegen(implicit indent: Int, context: CodeGenContext): String = {
    val b = new StringBuilder(s"${context.workingID}, create_args(${args.length}")

    if (args.isEmpty) {
      b.append(", NULL")
    }
    else {
      for (arg <- args) {
        val e = arg.expr.get
        b.append(s", ${TyperHelper.createVariable(e)}(${e.codegen})")
      }
    }

//    b.append(")")

    b.mkString
  }

  def tail(implicit indent: Int, context: CodeGenContext) = args.head.expr.get.codegen

  override def toString = s"TSecondCommandArgs(args = $args)"
}

final case class TCommandCall(id: TBodyContent, formalArgs: TFormalArgs, args: TArgs)(implicit override val scope: Scope) extends TSecondCommand with Typer {
  override def extractOfType(fst: Option[TOfType]): Option[TOfType] = id.ofType

  override def codegen(implicit indent: Int, context: CodeGenContext): String = {
    val b = new StringBuilder

    id match {
      case f: TFunction => b.append(s"${id.scope.currentClass.name}_${id.name}(${context.workingID}, create_args(${args.length}")
      case m: TMessage => b.append(s"${id.scope.currentClass.name}_${id.name}(${context.workingID}, create_args(${args.length}")
      case c: TConstructor => b.append(s"${id.scope.currentClass.name}_${id.name}(create_args(${args.length}")
      case a: TAmbient => b.append(s"${id.scope.currentClass.name}_${id.name}(${context.workingID}, create_args(${args.length}")
      case f: TField => throw new UnsupportedOperationException
      case d: TDelegate => throw new UnsupportedOperationException
    }

    if (args.isEmpty) {
      b.append(", NULL")
    }
    else {
      for (arg <- args) {
        val e = arg.expr.get
        b.append(s", ${TyperHelper.createVariable(e)}(${e.codegen})")
      }
    }

    b.append("))")

    b.mkString
  }

  override def toString = s"TCommandCall(id = $id, formalArgs = $formalArgs, args = $args)"

  def tail(implicit indent: Int, context: CodeGenContext) = throw new UnsupportedOperationException
}
