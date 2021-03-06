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
  def removeUnique(sc: Scope): Scope
  def id: Option[ID]
}

final case class TCommandExpr(expr: TExpr)(implicit val scope: Scope) extends TFirstCommand with Typer {
  override def extractOfType: Option[TOfType] = expr.ofType

  override def codegen(implicit indent: Int, context: CodeGenContext): String = "(" ++ expr.codegen ++ ")"

  override def tail(implicit indent: Int, context: CodeGenContext): String = expr.tail

  override def isSimple: Boolean = false

  override def toString = s"TCommandExpr(expr = $expr)"

  override def removeUnique(sc: Scope): Scope = sc

  override def id: Option[ID] = expr.id
}

final case class TCommandArgs(args: List[TArg])(implicit val scope: Scope) extends TFirstCommand with Typer {
  override def extractOfType: Option[TOfType] = ???

  override def codegen(implicit indent: Int, context: CodeGenContext): String = ???

  override def tail(implicit indent: Int, context: CodeGenContext): String = args.head.expr.get.tail

  override def isSimple: Boolean = false

  override def toString = s"TCommandArgs(args = $args)"

  override def removeUnique(sc: Scope): Scope = sc

  override def id: Option[ID] = None
}

sealed abstract class TAtom extends TFirstCommand with Typer {
  override def codegen(implicit indent: Int, context: CodeGenContext): String
  override def tail(implicit indent: Int, context: CodeGenContext): String = codegen
  override def isSimple: Boolean
  override def removeUnique(sc: Scope): Scope = sc
  def mode(implicit context: CodeGenContext): TMode = context.mode

  override def id: Option[ID] = None
}

final class TThis(implicit val scope: Scope) extends TAtom {
  override def extractOfType = None

  override def codegen(implicit indent: Int, context: CodeGenContext): String = "this"

  override def isSimple: Boolean = false

  override def toString = "TThis"

  override def id: Option[ID] = Some("this")
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
  override def id: Option[ID] = Some(i)

  override def extractOfType = scope.searchID(i)(this.pos)

  override def codegen(implicit indent: Int, context: CodeGenContext): String = {
    scope.findID(i) match {
      case Some(found) => found match {
        case v:Var => {
          if (v.inScope.isEmpty)
            i
          else if (v.inScope.get < pos)
            throw new VariableOutOfScope(i)
          else
            i
        }
        case m:Meth => s"${context.name}_$i"
      }
      case None => throw new VariableOutOfScope(i)
    }
  }

  override def isSimple: Boolean = {
    scope.findID(i) match {
      case Some(found) => found match {
        case v:Var => true
        case m:Meth => false
      }
      case None => throw new VariableOutOfScope(i)
    }
  }

  override def mode(implicit context: CodeGenContext): TMode = TyperHelper.mode(scope.searchID(i))


  override def removeUnique(sc: Scope): Scope = sc.removeScope(i)
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
