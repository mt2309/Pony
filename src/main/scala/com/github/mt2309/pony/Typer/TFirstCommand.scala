package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.Common._
import com.github.mt2309.pony.CodeGen.CodeGenContext

/**
 * User: mthorpe
 * Date: 22/06/2013
 * Time: 19:20
 */
sealed abstract class TFirstCommand extends Typer {
  def extractOfType: Option[TOfType]
  def tail(implicit indent: Int, context: CodeGenContext): String
  override def codegen(implicit indent: Int, context: CodeGenContext): String
  def isSimple: Boolean
}

final case class TCommandExpr(expr: TExpr)(implicit val scope: Scope) extends TFirstCommand with Typer {
  override def extractOfType: Option[TOfType] = expr.ofType

  override def codegen(implicit indent: Int, context: CodeGenContext): String = "(" ++ expr.codegen ++ ")"

  override def tail(implicit indent: Int, context: CodeGenContext): String = expr.tail

  override def isSimple: Boolean = false

  override def toString = s"TCommandExpr(expr = $expr)"
}

final case class TCommandArgs(args: List[TArg])(implicit val scope: Scope) extends TFirstCommand with Typer {
  override def extractOfType: Option[TOfType] = ???

  override def codegen(implicit indent: Int, context: CodeGenContext): String = ???

  override def tail(implicit indent: Int, context: CodeGenContext): String = args.head.expr.get.tail

  override def isSimple: Boolean = false

  override def toString = s"TCommandArgs(args = $args)"
}

sealed abstract class TAtom extends TFirstCommand with Typer {
  override def codegen(implicit indent: Int, context: CodeGenContext): String
  override def tail(implicit indent: Int, context: CodeGenContext): String = codegen
  override def isSimple: Boolean
}

final class TThis(implicit val scope: Scope) extends TAtom {
  override def extractOfType = None

  override def codegen(implicit indent: Int, context: CodeGenContext): String = "this"

  override def isSimple: Boolean = false

  override def toString = "TThis"
}

sealed abstract class TBoolean extends TAtom

final class TTrue(implicit val scope: Scope) extends TBoolean {
  override def extractOfType = Some(boolOfType)

  override def codegen(implicit indent: Int, context: CodeGenContext): String = "true"

  override def isSimple: Boolean = true

  override def toString = "TTrue"
}

final class  TFalse(implicit val scope: Scope) extends TBoolean {
  override def extractOfType = Some(boolOfType)

  override def codegen(implicit indent: Int, context: CodeGenContext): String = "false"

  override def isSimple: Boolean = true

  override def toString = "TFalse"
}

final case class TPonyInt(i: Int)(implicit val scope: Scope) extends TAtom with Typer {
  override def extractOfType = Some(intOfType)

  override def codegen(implicit indent: Int, context: CodeGenContext): String = i.toString

  override def isSimple: Boolean = true

  override def toString = s"Int($i)"
}

final case class TPonyDouble(d: Double)(implicit val scope: Scope) extends TAtom with Typer {
  override def extractOfType = Some(doubleOfType)

  override def codegen(implicit indent: Int, context: CodeGenContext): String = d.toString

  override def isSimple: Boolean = true

  override def toString = s"Double($d)"
}

final case class TPonyString(s: String)(implicit val scope: Scope) extends TAtom with Typer {
  override def extractOfType = Some(stringOfType)

  override def codegen(implicit indent: Int, context: CodeGenContext): String = "\"" ++ s ++ "\""

  override def isSimple: Boolean = true

  override def toString = s"String($s)"
}

final case class TPonyID(i: ID)(implicit val scope: Scope) extends TAtom with Typer {
  override def extractOfType = scope.searchID(i)(this.pos)

  override def codegen(implicit indent: Int, context: CodeGenContext): String = {
    scope.findID(i) match {
      case Some(found) => found match {
        case v:Var => i
        case m:Meth => s"${context.name}_$i"
      }
      case None => ???
    }
  }

  override def isSimple: Boolean = {
    scope.findID(i) match {
      case Some(found) => found match {
        case v:Var => true
        case m:Meth => false
      }
      case None => ???
    }
  }

}

final case class TPonyTypeId(t: TypeId)(implicit val scope: Scope) extends TAtom with Typer {
  override def extractOfType = {
    val search = scope.search(t)(this.pos)

    search match {
      case t: TPrimitive => Some(new TOfType(Set(t)))
      case t: TModuleMember => Some(new TOfType(Set(new TTypeClass(t))))
    }
  }

  override def isSimple: Boolean = false

  override def codegen(implicit indent: Int, context: CodeGenContext): String = "NULL"
}
