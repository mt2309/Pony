package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.Common._
import com.github.mt2309.pony.AST._
import util.parsing.input.{Position, Positional}
import annotation.tailrec
import com.github.mt2309.pony.CodeGen.CodeGenContext

/**
 * User: mthorpe
 * Date: 30/04/2013
 * Time: 00:57
 */


private[pony] trait Typer extends NotNull with Positional {
  def scope: Scope
  def codegen(implicit indent: Int, context: CodeGenContext): String
  implicit def curPos: Position = pos
}

final case class PreTypedModule(imports: CompilationUnits, classes: Map[TypeId, ModuleMember])(implicit val scope: Map[TypeId, ModuleMember], val filename: Filename) extends NotNull with Positional

final case class TypedModule(imports:CompilationUnits, classes: Map[TypeId, TModuleMember])(implicit val scope: Scope) extends Typer {
  override def codegen(implicit indent: Int, context: CodeGenContext): String = ???
}

final case class TParam(name: ID, ofType: Option[TOfType])(implicit val scope: Scope) {
  override def toString = s"TParam(name = $name, ofType = $ofType)"
}

final case class TCombinedArgs(formalArgs: FormalParams, args: TParams)(implicit val scope: Scope) extends Typer {
  override def codegen(implicit indent: Int, context: CodeGenContext): String = ???
}

final case class TArg(expr: Option[TExpr], ofType: Option[TOfType], assign: Option[TExpr])(implicit val scope: Scope) extends Typer {

  override def codegen(implicit indent: Int, context: CodeGenContext): String = expr.map(_.codegen).getOrElse("") ++ assign.map(_.codegen).getOrElse("")
}

final case class TIs(list: List[TTypeClass])(implicit val scope: Scope) extends Typer {
  def variables: Map[ID, Option[TOfType]] = {
    val vars = for (tClass <- list) yield tClass.variables

    TyperHelper.reduceVariables(vars)
  }

  def methods: Map[ID, TBodyContent] = {
    val bodies = for (tClass <- list) yield tClass.methods

    TyperHelper.reduceMethods(bodies)
  }

  def isSubType(that: TTypeElement): Boolean = list.contains(that)

  override def codegen(implicit indent: Int, context: CodeGenContext): String = ???

  override def toString = s"TIs(list = $list)"
}

final case class TDeclareMap(map: List[TPonyMap])(implicit val scope: Scope) extends Typer {
  override def codegen(implicit indent: Int, context: CodeGenContext): String = ???
}

final case class TPonyMap(from: TBodyContent, to: ID)(implicit val scope: Scope) extends Typer {
  override def codegen(implicit indent: Int, context: CodeGenContext): String = ???
}

final case class TOfType(typeList: Set[TTypeElement])(implicit val scope: Scope) extends Typer {

  override def codegen(implicit indent: Int, context: CodeGenContext): String = {
    if (this.isSubType(Some(primTOfType)))
      "0;"
    else
      typeList.head.defaultConstructor ++ ";"
  }

  def isSendable: Boolean = {
    (for (elem <- typeList) yield elem.mode match {
      case r: TReadOnly => false
      case i: TImmutable => true
      case m: TMutable => false
      case u: TUnique => true
      case TModeExpr(expr) => ???
    }).reduce(_ && _)
  }

  def isPrimitive: Boolean = isSubType(Some(primTOfType))

  def isEmpty: Boolean = typeList.exists(_.isInstanceOf[EmptyType])


  def maximalType: TPrimitive = {
    assert(isPrimitive)

    // oh god shoot me this is gonna be rough
    if (this.isSubType(Some(numericOfType))) {
      if (this.isSubType(Some(doubleOfType)))
        pDouble
      else if (this.isSubType(Some(intOfType)))
        pInt
      else
        pUInt
    }
    else {
      if (this.isSubType(Some(boolOfType))) {
        bool
      }
      else if (this.isSubType(Some(stringOfType)))
        pString
      else
        pChar
    }
  }

  def isSubType(that: Option[TOfType]): Boolean = {

    that match {
      case Some(get) => {
        (for (thisList <- this.typeList) yield {
          (for (thatList <- get.typeList) yield thatList == thisList || thisList.isSubType(thatList)).reduce(_ || _)}).reduce(_ && _)
      }
      case None => {
      // this.type
      (for (t <- this.typeList) yield scope.isList.exists(_.isSubType(t))).reduce(_ && _)
      }
    }
  }

  def mangle = {
    val b = new StringBuilder

    for (t <- typeList) b.append(s"${t.name}_")

    b.toString
  }

  def size = typeList.size

  def intersection(that: TOfType): TOfType = {
    val list = typeList.intersect(that.typeList)

    if (list.isEmpty) {
      doubleOfType
    }
    else {
      new TOfType(list)
    }
  }

  override def toString = s"TOfType(typelist = $typeList)"
}

final case class TTypeBody(body: Map[ID,TBodyContent])(implicit val scope: Scope) extends Typer {
  override def codegen(implicit indent: Int, context: CodeGenContext): String = ???

  override def toString = s"TTypeBody(body = $body)"
}

final case class TMethodContent(mode: TMode, id:ID, combinedArgs: TCombinedArgs)(implicit val scope: Scope) extends Typer {

  def args = combinedArgs.args
  def formal = combinedArgs.formalArgs

  override def codegen(implicit indent: Int, context: CodeGenContext): String = throw new UnsupportedOperationException

  override def toString = s"TMethodContent(id = $id, mode = $mode, args = $combinedArgs)"
}

final case class TExpr(unary: TUnary, operator: List[(Operator, TUnary)])(implicit val scope: Scope) extends Typer {

  def tail(implicit indent: Int, context: CodeGenContext): String = unary.tail

  def isSimple: Boolean = {
    if (operator.isEmpty) {
      unary.isSimple
    }
    else {
      true
    }
  }

  override def codegen(implicit indent: Int, context: CodeGenContext): String = {
    val u = unary.codegen
    if (operator.isEmpty)
      u
    else {
      val b = new StringBuilder(u)

      for (o <- operator) {
        o._1 match {
          case d: Divide => b.append(s" ${o._1.codeGen} (double)${o._2.codegen}")
          case _ => b.append(s" ${o._1.codeGen} ${o._2.codegen}")
        }
      }

      b.mkString
    }
  }

  def ofType(implicit scope: Scope): Option[TOfType] = {
    val unaryOfType = unary.extractOfType
    if (operator.isEmpty)
      unaryOfType
    else
      opList(unaryOfType, operator)
  }

  @tailrec
  private def opList(of: Option[TOfType], list: List[(Operator, TUnary)]): Option[TOfType] = list match {
    case x :: xs => {
      val rhsType = x._2.extractOfType.getOrElse(throw new TypeMismatch(primTOfType.toString, "this")(x._1.pos, scope))
      val lhsType = of.getOrElse(throw new TypeMismatch(primTOfType.toString, "this")(x._1.pos, scope))
      x._1 match {
        case t: NumericOp => {
          if (lhsType.isSubType(Some(numericOfType)) && rhsType.isSubType(Some(numericOfType)))
            opList(Some(rhsType.intersection(lhsType)), xs)
          else
            throw new TypeMismatch(numericOfType.toString, rhsType.toString)(x._1.pos, scope)
        }

        case t: BooleanOp => {
          if (lhsType.isSubType(Some(numericOfType)) && rhsType.isSubType(Some(numericOfType)))
            opList(Some(boolOfType), xs)
          else
            throw new TypeMismatch(boolOfType.toString, rhsType.toString)(x._1.pos, scope)
        }

        case t: NumericBooleanOp => {
          if (lhsType.isSubType(Some(primTOfType)) && rhsType.isSubType(Some(primTOfType)))
            opList(Some(boolOfType), xs)
          else
            throw new TypeMismatch(primTOfType.toString, rhsType.toString)(x._1.pos, scope)
        }

        case t: TypeOp => ???
      }
    }

    case Nil => of
  }

  override def toString = s"TExpr(unary = $unary, operator = $operator)"
}

final case class TCaseBlock(c: Option[TCaseSubBlock], block: TBlock)(implicit val scope: Scope) extends Typer {
  override def codegen(implicit indent: Int, context: CodeGenContext): String = ???
}

abstract class TCaseSubBlock(implicit val scope: Scope) extends Typer {
  override def codegen(implicit indent: Int, context: CodeGenContext): String = ???
}

final case class TCaseIf(expr: TExpr)(implicit override val scope: Scope) extends TCaseSubBlock {
  override def codegen(implicit indent: Int, context: CodeGenContext): String = {
    val b = new StringBuilder

    b.appendln(s"if (${expr.codegen})")

    b.mkString
  }
}

final case class TCaseVarList(varList: List[TCaseVar])(implicit override val scope: Scope) extends TCaseSubBlock {
  override def codegen(implicit indent: Int, context: CodeGenContext): String = ???
}

final case class TCaseVar(expr: Option[TExpr], forVar: TForVar)(implicit val scope: Scope) extends Typer {
  override def codegen(implicit indent: Int, context: CodeGenContext): String = ???
}

final case class TForVar(id: ID, ofType: Option[TOfType])(implicit val scope: Scope) extends Typer {
  override def codegen(implicit indent: Int, context: CodeGenContext): String = ???
}

sealed abstract class TLValue(implicit val scope: Scope) extends Typer {
  override def codegen(implicit indent: Int, context: CodeGenContext): String
  def constructor(implicit indent: Int, context: CodeGenContext): String
  def ofType: Option[TOfType]
}

final case class TLValueVar(nVar: TVarDec)(implicit override val scope: Scope) extends TLValue {
  override def codegen(implicit indent: Int, context: CodeGenContext): String = nVar.codegen

  override def constructor(implicit indent: Int, context: CodeGenContext) = nVar.constructor

  override def ofType: Option[TOfType] = nVar.ofType
}

final case class TLValueCommand(command: TCommand)(implicit override val scope: Scope) extends TLValue {
  override def codegen(implicit indent: Int, context: CodeGenContext): String = command.codegen

  override def constructor(implicit indent: Int, context: CodeGenContext) = command.constructor

  override def ofType: Option[TOfType] = command.extractOfType
}

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

final case class TCommand(first: TFirstCommand, second: Option[TSecondCommand])(implicit val scope: Scope) extends Typer {
  val extractOfType: Option[TOfType] = first.extractOfType

  def isSimple: Boolean = {
    if (second.isDefined) {
      false
    } else {
      first.isSimple
    }
  }

  override def codegen(implicit indent: Int, context: CodeGenContext): String = {
    val b = new StringBuilder

    first match {
      case TCommandExpr(expr) => second match {
        case Some(snd) => snd match {
          case TCommandCall(body, formal, args) => { b.append(); ??? }
          case TSecondCommandArgs(args) => { b.append(""); ??? }
        }
        case None      => { b.append(expr.codegen) }
      }
      case TCommandArgs(args) => second match {
        case Some(snd) => snd match {
          case TCommandCall(body, formal, tArgs) => { b.append(""); ??? }
          case TSecondCommandArgs(tArgs) => { b.append(""); ??? }
        }
        case None      => { b.append(""); ??? }
      }
      case t: TAtom           => second match {
        case Some(snd) => snd match {
          case tc: TCommandCall => {
            b.append(tc.codegen(indent, context.copy(workingID = t.codegen)))
          }
          case ts: TSecondCommandArgs => {
            b.append(t.codegen ++ "(" ++ ts.codegen ++ "))")
            b.append(TyperHelper.extractFrom(t))
          }
        }
        case None      => b.append(t.codegen)
      }
    }

    b.mkString
  }

  def constructor: String = ""

  def tail(implicit indent: Int, context: CodeGenContext) = first.tail

  override def toString = s"TCommand(first = $first, second = $second)"
}
