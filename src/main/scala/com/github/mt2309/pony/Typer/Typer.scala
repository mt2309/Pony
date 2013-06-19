package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.Common._
import com.github.mt2309.pony.AST._
import scala.util.parsing.input.Positional
import annotation.tailrec

/**
 * User: mthorpe
 * Date: 30/04/2013
 * Time: 00:57
 */


trait Typer extends NotNull with Positional {
  def scope: Scope
}

final case class PreTypedModule(imports: CompilationUnits, classes: Map[TypeId, ModuleMember])(implicit val scope: Map[TypeId, ModuleMember], val filename: Filename) extends NotNull with Positional

final case class TypedModule(imports:CompilationUnits, classes: Map[TypeId, TModuleMember])(implicit val scope: Scope) extends Typer

sealed abstract class TModuleMember(val name: TypeId)(implicit val scope: Scope) extends Typer {
  def methods: Map[ID, TBodyContent]
  def isSubType(that: TTypeElement): Boolean
  def variables: Map[ID, Option[TOfType]]
}

final case class EmptyType(override val name: TypeId)(implicit override val scope: Scope) extends TModuleMember(name) with TTypeElement {
  override def isSubType(that: TTypeElement) = this == that

  override def methods = Map.empty

  override def variables = Map.empty

  override def defaultConstructor = "NULL;\n"
}

final case class TPrimitive(typename: TypeId, cName: String, override val defaultConstructor: String)(implicit override val scope: Scope) extends TModuleMember(typename) with TTypeElement {

  override def isSubType(that: TTypeElement): Boolean = that match {
    case TPrimitive(pName, _ , _) => pName == typename
    case _ => false
  }

  def cTypeName: String = typename.toLowerCase

  override def variables = Map.empty

  override def methods = Map.empty

//  override def equals(that: Any): Boolean = that match {
//    case t:TPrimitive => this.typename == t.typename
//    case _ => false
//  }
}

final case class TDeclare(typename: TypeId, is: TIs, declareMap: TDeclareMap)(implicit override val scope: Scope) extends TModuleMember(typename) {

  override def methods = is.methods

  override def isSubType(that: TTypeElement): Boolean = ???

  override def variables = Map.empty
}

final case class TType(n: TypeId, ofType: TOfType, is: TIs)(implicit override val scope: Scope) extends TModuleMember(n) {
  override def methods = is.methods

  override def isSubType(that: TTypeElement): Boolean = ???

  override def variables = Map.empty
}

abstract class PonyClass(override val name: TypeId, val formalArgs: FormalParams, val is:TIs, val typeBody: TTypeBody)(implicit override val scope: Scope) extends TModuleMember(name) {

  override def isSubType(that: TTypeElement): Boolean = ???

  override def methods = {
    val methods = typeBody.body.filterNot(b => b._2.isInstanceOf[TField] || b._2.isInstanceOf[TField])
    TyperHelper.reduceMethods(List(methods, is.methods))
  }
}

abstract class ConcreteClass
(override val name: TypeId, override val formalArgs: FormalParams, override val is:TIs, override val typeBody: TTypeBody)(implicit override val scope: Scope) extends PonyClass(name, formalArgs, is, typeBody)
{
  def defaultConstructor: String = {
    val b = new StringBuilder(s"pony_clazz * ${name}__construct(pony_clazz ** args)\n{\n")

    for (variable <- variables) variable._2 match {
      case Some(of) => b.append(of)
      case None     => b.append(s"pony_clazz *")
    }

    b.append("}\n")
    b.mkString
  }

  override def variables = {
    val fields: Map[ID, Option[TOfType]] =    typeBody.body.filter(_._2.isInstanceOf[TField]).map(t =>    t._1 -> t._2.asInstanceOf[TField].ofType)
    val delegates: Map[ID, Option[TOfType]] = typeBody.body.filter(_._2.isInstanceOf[TDelegate]).map(t => t._1 -> t._2.asInstanceOf[TDelegate].ofType)


    TyperHelper.reduceVariables(List(fields, delegates))
  }
}

final case class TActor(n: TypeId, f: FormalParams, i:TIs, t: TTypeBody)(implicit override val scope: Scope)   extends ConcreteClass(n,f,i,t)
final case class TObject(n: TypeId, f: FormalParams, i:TIs, t: TTypeBody)(implicit override val scope: Scope)  extends ConcreteClass(n,f,i,t)
final case class TTrait(n: TypeId, f: FormalParams, i:TIs, t: TTypeBody)(implicit override val scope: Scope)   extends PonyClass(n,f,i,t) {
  override def variables = Map.empty
}

final case class TParam(name: ID, ofType: Option[TOfType])(implicit val scope: Scope)

final case class TCombinedArgs(formalArgs: FormalParams, args: TParams)(implicit val scope: Scope) extends Typer
final case class TArg(expr: Option[TExpr], ofType: Option[TOfType], assign: Option[TExpr])(implicit val scope: Scope) extends Typer {
  def codeGen: String = expr.map(_.codeGen).getOrElse("") ++ assign.map(_.codeGen).getOrElse("")
}

sealed trait TTypeElement extends Typer {
  def isSubType(that: TTypeElement): Boolean
  def name: String
  def defaultConstructor: String
}

final case class TPartialType(typeclass: TTypeClass)(implicit val scope: Scope) extends TTypeElement {
  def isSubType(that: TTypeElement): Boolean = that match {
    case t:TPrimitive => false
    case t:TPartialType => this.typeclass.isSubType(t.typeclass)
    case t:TTypeClass => this.typeclass.isSubType(t)
    case t:TLambda => false
    case t:EmptyType => false
  }

  def defaultConstructor = typeclass.defaultConstructor

  override def name = "partial_" ++ typeclass.name

  override def toString = "partial " ++ typeclass.moduleMember.name
}
final case class TTypeClass(moduleMember: TModuleMember, mode: TMode = new TReadOnly()(pScope), formalArgs: TFormalArgs = List.empty)(implicit val scope: Scope) extends TTypeElement {
  def isSubType(that: TTypeElement): Boolean = that match {
    case TPrimitive(name, _, _) => name == moduleMember.name
    case t:TPartialType => this.moduleMember.isSubType(t.typeclass)
    case t:TTypeClass => this.moduleMember.isSubType(t)
    case t:TLambda => false
    case t:EmptyType => false
  }

  def variables: Map[ID, Option[TOfType]] = moduleMember.variables

  def methods: Map[ID, TBodyContent] = moduleMember.methods

  override def toString = moduleMember.name

  override def name = moduleMember.name

  override def defaultConstructor = s"${moduleMember.name}_construct()"
}

final case class TLambda(mode: TMode, args: TArgs, result: TParams, throws: Boolean, block: Option[TBlock])(implicit val scope: Scope) extends TTypeElement {
  def isSubType(that: TTypeElement): Boolean = that match {
    case t:TPrimitive => false
    case t:TPartialType => false
    case t:TTypeClass => false
    case t:EmptyType => false
    case t:TLambda => {
      val arg = t.args.zip(this.args).map(t => t._1 == t._2).reduce(_ && _)
      val res = t.result.zip(this.result).map(t => t._1 == t._2).reduce(_ && _)

      arg && res
    }
  }

  override def defaultConstructor = ???

  override def name = "lambda_" ++ ArgsHelper.mangle(result)

  override def toString = "lambda " ++ args.toString ++ "->" ++ result.toString
}

sealed abstract class TMode(implicit val scope: Scope)              extends Typer
final case class TReadOnly(implicit override val scope: Scope)      extends TMode
final case class TImmutable(implicit override val scope: Scope)     extends TMode
final case class TMutable(implicit override val scope: Scope)       extends TMode
final case class TUnique(implicit override val scope: Scope)        extends TMode
final case class TModeExpr(expr: TExpr)(implicit override val scope: Scope) extends TMode


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
}
final case class TDeclareMap(map: List[TPonyMap])(implicit val scope: Scope) extends Typer
final case class TPonyMap(from: TBodyContent, to: ID)(implicit val scope: Scope) extends Typer

final case class TOfType(typeList: Set[TTypeElement])(implicit val scope: Scope) extends Typer {

  def defaultConstructor: String = {
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

  def intersection(that: TOfType): TOfType = ???
}

final case class TTypeBody(body: Map[ID,TBodyContent])(implicit val scope: Scope) extends Typer



final case class TMethodContent(mode: TMode, id:ID, combinedArgs: TCombinedArgs)(implicit val scope: Scope) extends Typer

final case class TExpr(unary: TUnary, operator: List[(Operator, TUnary)])(implicit val scope: Scope) extends Typer {

  def codeGen: String = {
    val u = unary.codeGen
    if (operator.isEmpty)
      u
    else {
      val b = new StringBuilder(u)

      for (o <- operator) {
        b.append(s" ${o._1.codeGen} ${o._2.codeGen}")
      }

      b.mkString
    }
  }

  def extractOfType(implicit scope: Scope): Option[TOfType] = {
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
            opList(Some(rhsType.intersection(of.get)), xs)
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
            opList(Some(rhsType), xs)
          else
            throw new TypeMismatch(primTOfType.toString, rhsType.toString)(x._1.pos, scope)
        }

        case t: TypeOp => ???
      }
    }

    case Nil => of
  }
}

trait TBlockContent extends Typer {
  def codeGen: String
}

final class TReturn(implicit val scope: Scope) extends TBlockContent {
  override def codeGen = "goto return_label;\n"
}

final class TThrow(implicit val scope: Scope) extends TBlockContent {
  override def codeGen = "\n"
}

final class TBreak(implicit val scope: Scope) extends TBlockContent {
  override def codeGen = "break;\n"
}

final class TContinue(implicit val scope: Scope) extends TBlockContent {
  override def codeGen = "continue;\n"
}

final case class TVarDec(id: ID, ofType: Option[TOfType], expr: Option[TExpr])(implicit val scope: Scope) extends TBlockContent {
  override def codeGen = {
    val b = new StringBuilder

    b.append(s"pony_clazz ** $id ")

    if (expr.isDefined)
      b.append(s"= ${expr.get.codeGen};\n")
    else
      b.append(s"= ${ofType.get.defaultConstructor};\n")

    b.mkString
  }

  def constructor: String = ofType.get.defaultConstructor
}

final case class TMatch(list: List[(TExpr,TCaseBlock)])(implicit val scope: Scope) extends TBlockContent {
  override def codeGen = {
    val b = new StringBuilder


    b.mkString
  }
}

final case class TDoLoop(block: TBlock, whileExpr: TExpr)(implicit val scope: Scope) extends TBlockContent {
  override def codeGen = s"do (${whileExpr.codeGen})\n${block.codeGen}\n"
}

final case class TWhileLoop(whileExpr: TExpr, block: TBlock)(implicit val scope: Scope) extends TBlockContent {
  override def codeGen = {
    val b = new StringBuilder
    b.append(s"while (${whileExpr.codeGen})\n")
    b.append(block.codeGen)

    b.append("\n")

    b.mkString
  }
}

final case class TForLoop(forVars: List[TForVar], inExpr: TExpr, block: TBlock)(implicit val scope: Scope) extends TBlockContent {
  override def codeGen = ???
}

final case class TConditional(conditionalList: List[(TExpr, TBlock)], elseBlock: Option[TBlock])(implicit val scope: Scope) extends TBlockContent {
  override def codeGen = {
    val builder = new StringBuilder
    builder.append(s"if (${conditionalList.head._1.codeGen})\n${conditionalList.head._2.codeGen}\n")

    for (cond <- conditionalList.tail)
      builder.append(s"else if (${cond._1})\n${cond._2}\n")

    for (e <- elseBlock)
      builder.append(s"else ${e.codeGen}")

    builder.mkString
  }
}

final case class TAssignment(lValues: List[TLValue], expr: Option[TExpr])(implicit val scope: Scope) extends TBlockContent {
  override def codeGen = {
    val b = new StringBuilder

    b.mkString
  }
}


final case class TCaseBlock(c: Option[TCaseSubBlock], block: TBlock)(implicit val scope: Scope) extends Typer
abstract class TCaseSubBlock(implicit val scope: Scope) extends Typer
final case class TCaseIf(expr: TExpr)(implicit override val scope: Scope) extends TCaseSubBlock
final case class TCaseVarList(varList: List[TCaseVar])(implicit override val scope: Scope) extends TCaseSubBlock

final case class TCaseVar(expr: Option[TExpr], forVar: TForVar)(implicit val scope: Scope) extends Typer
final case class TForVar(id: ID, ofType: Option[TOfType])(implicit val scope: Scope) extends Typer


sealed abstract class TLValue(implicit val scope: Scope) extends Typer {
  def codeGen: String
  def constructor: String
}

final case class TLValueVar(nVar: TVarDec)(implicit override val scope: Scope) extends TLValue {
  override def codeGen = nVar.codeGen

  override def constructor = nVar.constructor
}
final case class TLValueCommand(command: TCommand)(implicit override val scope: Scope) extends TLValue {
  override def codeGen = command.codeGen

  override def constructor = command.constructor
}

sealed abstract class TUnary(unaryOps: List[UnaryOp])(implicit val scope: Scope) extends Typer {
  def extractOfType: Option[TOfType]
  def codeGen: String
}

final case class TUnaryCommand(un: List[UnaryOp], command: TCommand)(implicit override val scope: Scope) extends TUnary(un) {
  override def extractOfType = command.extractOfType

  override def codeGen = {
    val b = new StringBuilder

    for (op <- un)
      b.append(op.codeGen)

    b.append(" " ++ command.codeGen)

    b.mkString
  }
}

final case class TUnaryLambda(un: List[UnaryOp], lambda: TLambda)(implicit override val scope: Scope) extends TUnary(un) {
  override def extractOfType = Some(new TOfType(Set(lambda)))

  override def codeGen = ???
}

final case class TCommand(first: TFirstCommand, second: Option[TSecondCommand])(implicit val scope: Scope) extends Typer {
  def extractOfType: Option[TOfType] = {
    if (second.isEmpty) {
      first.extractOfType
    }
    else {
      val fst = first.extractOfType

      second.get.extractOfType(fst)
    }
  }

  def codeGen: String = s"${first.codeGen}${if (second.isDefined) second.get.codeGen else ""}"

  def constructor: String = ""
}

sealed abstract class TFirstCommand extends Typer {
  def extractOfType: Option[TOfType]
  def codeGen: String
}

final case class TCommandExpr(expr: TExpr)(implicit val scope: Scope) extends TFirstCommand with Typer {
  override def extractOfType: Option[TOfType] = expr.extractOfType

  override def codeGen = ???
}

final case class TCommandArgs(args: List[TArg])(implicit val scope: Scope) extends TFirstCommand with Typer {
  override def extractOfType: Option[TOfType] = ???

  override def codeGen = ???
}

sealed abstract class TAtom extends TFirstCommand with Typer

final class TThis(implicit val scope: Scope) extends TAtom {
  override def extractOfType = None

  override def codeGen = "this"
}

final class  TTrue(implicit val scope: Scope) extends TAtom {
  override def extractOfType = Some(boolOfType)

  override def codeGen = "true"
}

final class  TFalse(implicit val scope: Scope) extends TAtom {
  override def extractOfType = Some(boolOfType)

  override def codeGen = "false"
}

final case class TPonyInt(i: Int)(implicit val scope: Scope) extends TAtom with Typer {
  override def extractOfType = Some(intOfType)

  override def codeGen = i.toString
}

final case class TPonyDouble(d: Double)(implicit val scope: Scope) extends TAtom with Typer {
  override def extractOfType = Some(doubleOfType)

  override def codeGen = d.toString
}

final case class TPonyString(s: String)(implicit val scope: Scope) extends TAtom with Typer {
  override def extractOfType = Some(stringOfType)

  override def codeGen = "\"" ++ s ++ "\""
}

final case class TPonyID(i: ID)(implicit val scope: Scope) extends TAtom with Typer {
  override def extractOfType = scope.searchID(i)(this.pos)

  override def codeGen = i
}

final case class TPonyTypeId(t: TypeId)(implicit val scope: Scope, implicit val checker: LowerTypeChecker) extends TAtom with Typer {
  override def extractOfType = Some(new TOfType(Set(new TTypeClass(scope.search(t, checker)(this.pos)))))

  override def codeGen = t
}

sealed abstract class TSecondCommand(implicit val scope: Scope) extends Typer {
  def extractOfType(fst: Option[TOfType]): Option[TOfType]
  def codeGen: String
}

final case class TSecondCommandArgs(args: TArgs)(implicit override val scope: Scope) extends TSecondCommand with Typer {
  override def extractOfType(fst: Option[TOfType]): Option[TOfType] = ???

  override def codeGen = ???
}

final case class TCommandCall(id: TBodyContent, formalArgs: TFormalArgs, args: TArgs)(implicit override val scope: Scope) extends TSecondCommand with Typer {
  override def extractOfType(fst: Option[TOfType]): Option[TOfType] = ???

  override def codeGen = s"${id.scope.currentClass.name}_${id.name}(this, ${ArgsHelper.codeGen(args)}})"
}
