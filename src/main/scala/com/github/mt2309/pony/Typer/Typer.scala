package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.Common._
import com.github.mt2309.pony.AST._
import util.parsing.input.Positional
import annotation.tailrec
import com.github.mt2309.pony.CodeGen.CodeGenContext

/**
 * User: mthorpe
 * Date: 30/04/2013
 * Time: 00:57
 */


trait Typer extends NotNull with Positional {
  def scope: Scope
  def codegen(implicit indent: Int, context: CodeGenContext): String
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


final case class TIs(list: List[TTypeClass])(implicit val scope: Scope) extends Typer {
  def variables: Map[ID, Option[TOfType]] = {
    val vars = for (tClass <- list) yield tClass.variables

    TyperHelper.reduceVariables(vars)
  }

  def methods: Map[ID, TBodyContent] = {
    val bodies = for (tClass <- list) yield tClass.methods

    TyperHelper.reduceMethods(bodies)
  }

  def isSubType(that: TTypeElement): Boolean = ???

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
      "0;\n"
    else
      typeList.head.defaultConstructor ++ ";\n"
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

    if (that.isDefined) {
      (for (t <- this.typeList) yield {
        (for (tt <- that.get.typeList) yield tt == t || t.isSubType(tt)).reduce(_ || _)
      }).reduce(_ && _)
    } else {
      // this.type
      (for (t <- this.typeList) yield scope.isList.exists(_.isSubType(t))).reduce(_ && _)
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
        b.append(s" ${o._1.codeGen} ${o._2.codegen}")
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
  def extractOfType: Option[TOfType]
  def isSimple: Boolean
  def tail(implicit indent: Int, context: CodeGenContext): String
  override def codegen(implicit indent: Int, context: CodeGenContext): String

  def unaryOps: List[UnaryOp]
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
  override def extractOfType = Some(new TOfType(Set(lambda)))

  override def codegen(implicit indent: Int, context: CodeGenContext): String = ???

  override def tail(implicit indent: Int, context: CodeGenContext): String = throw new UnsupportedOperationException

  override def isSimple: Boolean = false
}

final case class TCommand(first: TFirstCommand, second: Option[TSecondCommand])(implicit val scope: Scope) extends Typer {
  def extractOfType: Option[TOfType] = first.extractOfType

  def isSimple: Boolean = {
    if (second.isDefined) {
      false
    } else {
      first.isSimple
    }
  }

  override def codegen(implicit indent: Int, context: CodeGenContext): String = {
    val b = new StringBuilder

    b.append(first.codegen)

    second.map(m => b.append("(" ++ m.codegen ++ ")"))
    second.map(m => TyperHelper.extractFrom(m.extractOfType(extractOfType)))

    b.mkString
  }

  def constructor: String = ""

  def tail(implicit indent: Int, context: CodeGenContext) = first.tail

  override def toString = s"TCommand(first = $first, second = $second)"
}


sealed abstract class TSecondCommand(implicit val scope: Scope) extends Typer {
  def extractOfType(fst: Option[TOfType]): Option[TOfType]
  def tail(implicit indent: Int, context: CodeGenContext): String
  override def codegen(implicit indent: Int, context: CodeGenContext): String
}

final case class TSecondCommandArgs(args: TArgs)(implicit override val scope: Scope) extends TSecondCommand with Typer {
  override def extractOfType(fst: Option[TOfType]): Option[TOfType] = fst

  override def codegen(implicit indent: Int, context: CodeGenContext): String = {
    val b = new StringBuilder(s"this, create_args(${args.length}")

    if (args.isEmpty) {
      b.append(", NULL")
    }
    else {
      for (arg <- args) {
        b.append(s", ${TyperHelper.createVariable(arg.ofType)}(${arg.expr.get.codegen})")
      }
    }

    b.append(")")

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
      case f: TFunction => b.append(s"${id.scope.currentClass.name}_${id.name}(this, create_args(${args.length}")
      case m: TMessage => b.append(s"${id.scope.currentClass.name}_${id.name}(this, create_args(${args.length}")
      case c: TConstructor => b.append(s"${id.scope.currentClass.name}_${id.name}(create_args(${args.length}")
      case a: TAmbient => b.append(s"${id.scope.currentClass.name}_${id.name}(this, create_args(${args.length}")
      case f: TField => throw new UnsupportedOperationException
      case d: TDelegate => throw new UnsupportedOperationException
    }

    if (args.isEmpty) {
      b.append(", NULL")
    }
    else {
      for (arg <- args) {
        val e = arg.expr.get
        b.append(s", ${TyperHelper.createVariable(e.ofType)}(${e.codegen})")
      }
    }

    b.append(")")

    b.mkString
  }

  override def toString = s"TCommandCall(id = $id, formalArgs = $formalArgs, args = $args)"

  def tail(implicit indent: Int, context: CodeGenContext) = throw new UnsupportedOperationException
}
