package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.Common._
import com.github.mt2309.pony.AST._
import scala.util.parsing.input.{Position, Positional}

/**
 * User: mthorpe
 * Date: 30/04/2013
 * Time: 00:57
 */


sealed trait Typer extends NotNull with Positional

final case class PreTypedModule(imports: CompilationUnits, classes: Map[TypeId, ModuleMember])(implicit val scope: Map[TypeId, ModuleMember], val filename: Filename) extends Typer

final case class TypedModule(imports:CompilationUnits, classes: Map[TypeId, TModuleMember])(implicit val scope: Scope) extends Typer

sealed abstract class TModuleMember(val name: TypeId)(implicit val scope: Scope) extends Typer

final case class TPrimitive(typename: TypeId)(implicit override val scope: Scope) extends TModuleMember(typename) with TTypeElement {
  def toIPrim: IPrimitive = new IPrimitive(typename)
}

final case class TDeclare(typename: TypeId, is: TIs, declareMap: TDeclareMap)(implicit override val scope: Scope) extends TModuleMember(typename)
final case class TType(n: TypeId, ofType: TOfType, is: TIs)(implicit override val scope: Scope) extends TModuleMember(n)

abstract class PonyClass(val na: TypeId, val formalArgs: TFormalArgs, val is:TIs, val typeBody: TTypeBody)(implicit override val scope: Scope) extends TModuleMember(na)
final case class TActor(n: TypeId, f: TFormalArgs, i:TIs, t: TTypeBody)(implicit override val scope: Scope)   extends PonyClass(n,f,i,t)
final case class TTrait(n: TypeId, f: TFormalArgs, i:TIs, t: TTypeBody)(implicit override val scope: Scope)   extends PonyClass(n,f,i,t)
final case class TObject(n: TypeId, f: TFormalArgs, i:TIs, t: TTypeBody)(implicit override val scope: Scope)  extends PonyClass(n,f,i,t)

final case class TParam(name: ID, ofType: TOfType)

final case class TCombinedArgs(formalArgs: TFormalArgs, args: TParams)(implicit val scope: Scope) extends Typer
final case class TArg(expr: Option[TExpr], ofType: Option[TOfType], assign: Option[TExpr])(implicit val scope: Scope) extends Typer

sealed trait TTypeElement extends Typer
final case class TPartialType(name: TTypeClass)(implicit val scope: Scope) extends TTypeElement
final case class TTypeClass(moduleMember: IModuleMember, mode: TMode = TReadOnly, formalArgs: TFormalArgs = List.empty)(implicit val scope: Scope) extends TTypeElement
final case class TLambda(mode: TMode, args: TArgs, result: TParams, throws: Boolean, block: Option[TBlock])(implicit val scope: Scope) extends TTypeElement

sealed abstract class TMode extends Typer
object TReadOnly    extends TMode
object TImmutable    extends TMode
object TMutable      extends TMode
object TUnique       extends TMode
final case class TModeExpr(expr: TExpr) extends TMode

final case class TBlock(contents:List[TBlockContent], catchBlock: Option[TBlock], alwaysBlock: Option[TBlock])(implicit val scope: Scope) extends TBlockContent with Typer
final case class TIs(list: List[TTypeClass])(implicit val scope: Scope) extends Typer
final case class TDeclareMap(map: List[TPonyMap])(implicit val scope: Scope) extends Typer
final case class TPonyMap(from:BodyContent, to: ID)(implicit val scope: Scope) extends Typer

final case class TOfType(typeList: Set[TTypeElement])(implicit val scope: Scope) extends Typer


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
    if (operator.isEmpty)
      unary.extractOfType
    else
      numericOfType
  }
}

trait TBlockContent extends Typer

object TReturn extends TBlockContent
object TThrow extends TBlockContent
object TBreak extends TBlockContent
object TContinue extends TBlockContent

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
  def extractOfType(implicit scope: Scope): TOfType
}

final case class TUnaryCommand(un: List[UnaryOp], command: TCommand)(implicit override val scope: Scope) extends TUnary(un) {
  def extractOfType(implicit scope: Scope): TOfType = ???
}

final case class TUnaryLambda(un: List[UnaryOp], lambda: TLambda)(implicit override val scope: Scope) extends TUnary(un) {
  def extractOfType(implicit scope: Scope): TOfType = ???
}

final case class TCommand(first: TFirstCommand, second: Option[TSecondCommand])(implicit val scope: Scope) extends Typer

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
    val cur = scope.currentClass.getOrElse(throw new ThisUsedOutsideClassException()(this.pos))
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
  def extractOfType = scope.searchID(i)(this.pos)
}

final case class TPonyTypeId(t: TypeId)(implicit val scope: Scope) extends TAtom with Typer {
  def extractOfType:TOfType = new TOfType(Set(new TTypeClass(scope.search(t)(this.pos))))
}


sealed abstract class TSecondCommand extends Typer
final case class TSecondCommandArgs(args: TArgs) extends TSecondCommand with Typer
final case class TCommandCall(id: BodyContent, formalArgs: TFormalArgs, args: TArgs) extends TSecondCommand with Typer