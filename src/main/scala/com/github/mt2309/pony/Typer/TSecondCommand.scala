package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.CodeGen.CodeGenContext
import com.github.mt2309.pony.AST

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

    b.mkString
  }

  def tail(implicit indent: Int, context: CodeGenContext) = args.head.expr.get.codegen

  override def toString = s"TSecondCommandArgs(args = $args)"
}

final case class TCommandCall(id: TBodyContent, formalArgs: TFormalArgs, args: TArgs)(implicit override val scope: Scope) extends TSecondCommand with Typer {

  id match {
    case m: TInstanceVariable => throw new UnsupportedOperationException
    case v: TMethod => {
      if (v.formalArgs.length == formalArgs.length) {
        val zipArgs: List[(TParam, TArg)] = v.args.zip(args)
        for (arg <- zipArgs) {
          if (!TyperHelper.subType(arg._2.expr.get.ofType, arg._1.ofType)) {
            throw new TypeMismatch(arg._2.expr.get.ofType.toString, arg._1.ofType.toString)
          }
        }
      }
      else {
        throw new ArgumentMismatchException(v)
      }
    }
  }


  override def extractOfType(fst: Option[TOfType]): Option[TOfType] = id.ofType

  override def codegen(implicit indent: Int, context: CodeGenContext): String = {

    if (!context.mode.isSubType(id.mode)) {
      throw new ModeMismatchException(context.mode, id.mode)
    }

    val b = new StringBuilder
    val e = new StringBuilder

    id match {
      case f: TFunction => {
        val cur = id.scope.currentClass.currentClass.getOrElse(new AST.Object(context.workingID,List.empty, new AST.Is(List.empty), new AST.TypeBody(Map.empty), false)(scope.filename))
        b.append(s"${cur.typeName}_${id.name}(${context.workingID}, create_args(${args.length}")
      }
      case m: TMessage => {
        b.append(s"pony_send((actor_t*)${context.workingID}, ${id.name.hashCode.abs}, create_args(${args.length}")
        e.append(", NULL")
      }
      case c: TConstructor => {
        val cur = id.scope.currentClass.currentClass.getOrElse(new AST.Object(context.workingID,List.empty, new AST.Is(List.empty), new AST.TypeBody(Map.empty), false)(scope.filename))
        cur match {
          case a: AST.Actor => {
            b.append(s"(pony_clazz*)pony_send(pony_create(${cur.typeName}_dispatch, NULL), ${id.name.hashCode.abs}, create_args(${args.length}")
            e.append(s", NULL")
          }
          case o: AST.Object => {
            b.append(s"${cur.typeName}_${id.name}(create_args(${args.length}")
          }
          case _ => throw new RuntimeException
        }
      }
      case a: TAmbient => b.append(s"${id.scope.currentClass.name}_${id.name}(${context.workingID}, create_args(${args.length}")
      case f: TField => throw new UnsupportedOperationException
      case d: TDelegate => throw new UnsupportedOperationException
    }

    if (args.isEmpty) {
      b.append(", NULL")
    }
    else {
      for (arg <- args; e <- arg.expr) {
        b.append(s", ${TyperHelper.createVariable(e)}(${e.codegen})")
      }
    }

    b.append(")")
    b.append(e)
    b.append(")")

    b.mkString
  }

  override def toString = s"TCommandCall(id = $id, formalArgs = $formalArgs, args = $args)"

  def tail(implicit indent: Int, context: CodeGenContext) = throw new UnsupportedOperationException
}
