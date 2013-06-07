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


sealed trait Typer extends NotNull with Positional {
  def scope: Scope

  def codeGen: AnyRef = ???
}

final case class PreTypedModule(imports: CompilationUnits, classes: Map[TypeId, ModuleMember])(implicit val scope: Map[TypeId, ModuleMember], val filename: Filename) extends NotNull with Positional

final case class TypedModule(imports:CompilationUnits, classes: Map[TypeId, TModuleMember])(implicit val scope: Scope) extends Typer

sealed abstract class TModuleMember(val name: TypeId)(implicit val scope: Scope) extends Typer

final case class TPrimitive(typename: TypeId)(implicit override val scope: Scope) extends TModuleMember(typename) with TTypeElement {
  def toIPrim: IPrimitive = new IPrimitive(typename)
  override def isSubType(that: TTypeElement): Boolean = that match {
    case TPrimitive(name) => name == typename
    case _ => false
  }

  override def equals(that: Any): Boolean = that match {
    case t:TPrimitive => this.typename == t.typename
    case _ => false
  }
}

final case class TDeclare(typename: TypeId, is: TIs, declareMap: TDeclareMap)(implicit override val scope: Scope) extends TModuleMember(typename)
final case class TType(n: TypeId, ofType: TOfType, is: TIs)(implicit override val scope: Scope) extends TModuleMember(n)

abstract class PonyClass(val na: TypeId, val formalArgs: TFormalArgs, val is:TIs, val typeBody: TTypeBody)(implicit override val scope: Scope) extends TModuleMember(na)
final case class TActor(n: TypeId, f: TFormalArgs, i:TIs, t: TTypeBody)(implicit override val scope: Scope)   extends PonyClass(n,f,i,t)
final case class TTrait(n: TypeId, f: TFormalArgs, i:TIs, t: TTypeBody)(implicit override val scope: Scope)   extends PonyClass(n,f,i,t)
final case class TObject(n: TypeId, f: TFormalArgs, i:TIs, t: TTypeBody)(implicit override val scope: Scope)  extends PonyClass(n,f,i,t)

final case class TParam(name: ID, ofType: TOfType)(implicit val scope: Scope)

final case class TCombinedArgs(formalArgs: FormalParams, args: TParams)(implicit val scope: Scope) extends Typer
final case class TArg(expr: Option[TExpr], ofType: Option[TOfType], assign: Option[TExpr])(implicit val scope: Scope) extends Typer

sealed trait TTypeElement extends Typer {
  def isSubType(that: TTypeElement): Boolean
}

final case class TPartialType(name: TTypeClass)(implicit val scope: Scope) extends TTypeElement {
  def isSubType(that: TTypeElement): Boolean = that match {
    case t:TPrimitive => false
    case t:TPartialType => this.name.isSubType(t.name)
    case t:TTypeClass => this.name.isSubType(t)
    case t:TLambda => false
  }

  override def toString = "partial " ++ name.moduleMember.name
}
final case class TTypeClass(moduleMember: IModuleMember, mode: TMode = new TReadOnly()(pScope), formalArgs: TFormalArgs = List.empty)(implicit val scope: Scope) extends TTypeElement {
  def isSubType(that: TTypeElement): Boolean = that match {
    case TPrimitive(name) => name == moduleMember.name
    case t:TPartialType => this.moduleMember.isSubType(t.name.moduleMember)
    case t:TTypeClass => this.moduleMember.isSubType(t.moduleMember)
    case t:TLambda => false
  }

  def getVars: Map[ID, OfType] = moduleMember.getVariables

  override def toString = moduleMember.name
}
final case class TLambda(mode: TMode, args: TArgs, result: TParams, throws: Boolean, block: Option[TBlock])(implicit val scope: Scope) extends TTypeElement {
  def isSubType(that: TTypeElement): Boolean = that match {
    case t:TPrimitive => false
    case t:TPartialType => false
    case t:TTypeClass => false
    case t:TLambda => {
      val arg = t.args.zip(this.args).map(t => t._1 == t._2).reduce(_ && _)
      val res = t.result.zip(this.result).map(t => t._1 == t._2).reduce(_ && _)

      arg && res
    }
  }

  override def toString = "lambda " ++ args.toString ++ "->" ++ result.toString
}

sealed abstract class TMode(implicit val scope: Scope)              extends Typer
final case class TReadOnly(implicit override val scope: Scope)      extends TMode
final case class TImmutable(implicit override val scope: Scope)     extends TMode
final case class TMutable(implicit override val scope: Scope)       extends TMode
final case class TUnique(implicit override val scope: Scope)        extends TMode
final case class TModeExpr(expr: TExpr)(implicit override val scope: Scope) extends TMode

final case class TBlock(contents:List[TBlockContent], catchBlock: Option[TBlock], alwaysBlock: Option[TBlock])(implicit val scope: Scope) extends TBlockContent with Typer
final case class TIs(list: List[TTypeClass])(implicit val scope: Scope) extends Typer {
  def getVariables: Map[ID, OfType] = {
    val vars = for (tClass <- list) yield tClass.getVars
    var m: Map[ID, OfType] = Map.empty
    for (v <- vars; varD <- v) {
      if (!m.contains(varD._1))
        m += varD
    }

    m
  }
}
final case class TDeclareMap(map: List[TPonyMap])(implicit val scope: Scope) extends Typer
final case class TPonyMap(from:BodyContent, to: ID)(implicit val scope: Scope) extends Typer

final case class TOfType(typeList: Set[TTypeElement])(implicit val scope: Scope) extends Typer {

  def isSubType(that: TOfType): Boolean = {
    (for (t <- this.typeList) yield {
      (for (tt <- that.typeList) yield (tt == t || t.isSubType(tt))).reduce(_ || _)
    }).reduce(_ && _)
  }

  def intersection(that: TOfType): TOfType = ???
}

final case class TTypeBody(body: Map[ID,TBodyContent])(implicit val scope: Scope) extends Typer

sealed abstract class TBodyContent(val name: ID, val isAbstract: Boolean = false)(implicit val scope: Scope) extends Typer
final case class TField(id: ID, ofType: TOfType, expr: Option[TExpr])(implicit override val scope: Scope) extends TBodyContent(id, expr.isEmpty)
final case class TDelegate(id: ID, ofType: TOfType)(implicit override val scope: Scope) extends TBodyContent(name = id)
final case class TConstructor(contents: TMethodContent, throws: Boolean, block: Option[TBlock])(implicit override val scope: Scope) extends TBodyContent(contents.id, block.isEmpty)
final case class TAmbient(contents: TMethodContent, throws: Boolean, block: Option[TBlock])(implicit override val scope: Scope) extends TBodyContent(contents.id, block.isEmpty)
final case class TFunction(contents: TMethodContent, results: TParams, throws: Boolean, block: Option[TBlock])(implicit override val scope: Scope) extends TBodyContent(contents.id, block.isEmpty)
final case class TMessage(contents: TMethodContent, block: Option[TBlock])(implicit override val scope: Scope) extends TBodyContent(contents.id, block.isEmpty)

final case class TMethodContent(mode: TMode, id:ID, combinedArgs: TCombinedArgs)(implicit val scope: Scope) extends Typer

final case class TExpr(unary: TUnary, operator: List[(Operator, TUnary)])(implicit val scope: Scope) extends Typer {

  def extractOfType(implicit scope: Scope): TOfType = {
    val unaryOfType = unary.extractOfType
    if (operator.isEmpty)
      unaryOfType
    else
      opList(unaryOfType, operator)
  }

  @tailrec
  private def opList(of: TOfType, list: List[(Operator, TUnary)]): TOfType = list match {
    case x :: xs => {
      val rhsType = x._2.extractOfType
      x._1 match {
        case t: NumericOp => {
          if (of.isSubType(numericOfType) && rhsType.isSubType(numericOfType))
            opList(rhsType.intersection(of), xs)
          else
            throw new TypeMismatch(numericOfType.toString, rhsType.toString)(x._1.pos, scope)
        }

        case t: BooleanOp => {
          if (of.isSubType(numericOfType) && rhsType.isSubType(numericOfType))
            opList(boolOfType, xs)
          else
            throw new TypeMismatch(boolOfType.toString, rhsType.toString)(x._1.pos, scope)
        }

        case t: NumericBooleanOp => {
          if (of.isSubType(primTOfType) && rhsType.isSubType(primTOfType))
            opList(rhsType, xs)
          else
            throw new TypeMismatch(primTOfType.toString, rhsType.toString)(x._1.pos, scope)
        }

        case t: TypeOp => ???
      }
    }

    case Nil => of
  }
}

trait TBlockContent extends Typer

final class TReturn(implicit val scope: Scope) extends TBlockContent
final class TThrow(implicit val scope: Scope) extends TBlockContent
final class TBreak(implicit val scope: Scope) extends TBlockContent
final class TContinue(implicit val scope: Scope) extends TBlockContent

final case class TVarDec(id: ID, ofType: TOfType, expr: Option[TExpr])(implicit val scope: Scope) extends TBlockContent
final case class TMatch(list: List[TExpr], cases: List[TCaseBlock])(implicit val scope: Scope) extends TBlockContent
final case class TDoLoop(block: TBlock, whileExpr: TExpr)(implicit val scope: Scope) extends TBlockContent
final case class TWhileLoop(whileExpr: TExpr, block: TBlock)(implicit val scope: Scope) extends TBlockContent
final case class TForLoop(forVars: List[TForVar], inExpr: TExpr, block: TBlock)(implicit val scope: Scope) extends TBlockContent
final case class TConditional(conditionalList: List[(TExpr, TBlock)], elseBlock: Option[TBlock])(implicit val scope: Scope) extends TBlockContent
final case class TAssignment(lValues: List[TLValue], expr: Option[TExpr])(implicit val scope: Scope) extends TBlockContent

final case class TCaseBlock(c: Option[TCaseSubBlock], block: TBlock)(implicit val scope: Scope) extends Typer
abstract class TCaseSubBlock(implicit val scope: Scope) extends Typer
final case class TCaseIf(expr: TExpr)(implicit override val scope: Scope) extends TCaseSubBlock
final case class TCaseVarList(varList: List[TCaseVar])(implicit override val scope: Scope) extends TCaseSubBlock

final case class TCaseVar(expr: Option[TExpr], forVar: TForVar)(implicit val scope: Scope) extends Typer
final case class TForVar(id: ID, ofType: TOfType)(implicit val scope: Scope) extends Typer


sealed abstract class TLValue(implicit val scope: Scope) extends Typer
final case class TLValueVar(nVar: TVarDec)(implicit override val scope: Scope) extends TLValue
final case class TLValueCommand(command: TCommand)(implicit override val scope: Scope) extends TLValue

sealed abstract class TUnary(unaryOps: List[UnaryOp])(implicit val scope: Scope) extends Typer {
  def extractOfType: TOfType
}

final case class TUnaryCommand(un: List[UnaryOp], command: TCommand)(implicit override val scope: Scope) extends TUnary(un) {
  def extractOfType: TOfType = command.extractOfType
}

final case class TUnaryLambda(un: List[UnaryOp], lambda: TLambda)(implicit override val scope: Scope) extends TUnary(un) {
  def extractOfType: TOfType = ???
}

final case class TCommand(first: TFirstCommand, second: Option[TSecondCommand])(implicit val scope: Scope) extends Typer {
  def extractOfType: TOfType = {
    if (second.isEmpty) first.extractOfType
    else ???
  }
}

sealed abstract class TFirstCommand extends Typer {
  def extractOfType: TOfType
}

final case class TCommandExpr(expr: TExpr)(implicit val scope: Scope) extends TFirstCommand with Typer {
  def extractOfType: TOfType = expr.extractOfType
}
final case class TCommandArgs(args: List[TArg])(implicit val scope: Scope) extends TFirstCommand with Typer {
  def extractOfType: TOfType = ???
}
sealed abstract class TAtom extends TFirstCommand with Typer

final class TThis(implicit val scope: Scope) extends TAtom{
  def extractOfType: TOfType = {
    val cur = scope.currentClass.currentClass.getOrElse(throw new ThisUsedOutsideClassException()(this.pos, scope))
    new TOfType(Set(new TTypeClass(cur)))
  }
}

final class  TTrue(implicit val scope: Scope) extends TAtom{
  def extractOfType = boolOfType
}

final class  TFalse(implicit val scope: Scope) extends TAtom {
  def extractOfType = boolOfType
}

final case class TPonyInt(i: Int)(implicit val scope: Scope) extends TAtom with Typer{
  def extractOfType = intOfType
}

final case class TPonyDouble(d: Double)(implicit val scope: Scope) extends TAtom with Typer{
  def extractOfType = doubleOfType
}

final case class TPonyString(s: String)(implicit val scope: Scope) extends TAtom with Typer{
  def extractOfType = ???
}

final case class TPonyID(i: ID)(implicit val scope: Scope) extends TAtom with Typer{
  def extractOfType = {
    scope.searchID(i)(this.pos)
  }
}

final case class TPonyTypeId(t: TypeId)(implicit val scope: Scope) extends TAtom with Typer {
  def extractOfType:TOfType = new TOfType(Set(new TTypeClass(scope.search(t)(this.pos))))
}


sealed abstract class TSecondCommand(implicit val scope: Scope) extends Typer
final case class TSecondCommandArgs(args: TArgs)(implicit override val scope: Scope) extends TSecondCommand with Typer
final case class TCommandCall(id: BodyContent, formalArgs: TFormalArgs, args: TArgs)(implicit override val scope: Scope) extends TSecondCommand with Typer

object TyperHelper {
  private var ofTypeCache: Map[IOfType, TOfType] = Map.empty
  private var isCache: Map[IIs, TIs] = Map.empty
  private var moduleCache: Map[IModuleMember, TModuleMember] = Map.empty
  private var typeclassCache: Map[TypeElement, TTypeElement] = Map.empty


  def lookupIs(is: IIs): Option[TIs] = isCache.get(is)
  def updateIs(is: IIs, tIs: TIs): Unit = isCache += is -> tIs

  def lookupOf(of: IOfType): Option[TOfType] = ofTypeCache.get(of)
  def updateOfType(of: IOfType, tOf: TOfType): Unit = ofTypeCache += of -> tOf

  def lookupModule(i: IModuleMember): Option[TModuleMember] = moduleCache.get(i)
  def updateModules(i: IModuleMember, t: TModuleMember): Unit = moduleCache += i -> t

  def lookupTypeclass(t: TypeElement): Option[TTypeElement] = typeclassCache.get(t)

  def updateTypeclass(t: TypeElement, tt: TTypeElement): Unit = typeclassCache += t -> tt
}
