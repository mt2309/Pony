package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.Common._
import com.github.mt2309.pony.AST.{ModuleMember, UnaryOp, Operator}

/**
 * User: mthorpe
 * Date: 30/04/2013
 * Time: 00:57
 */

//final case class TypedModule(imports: CompilationUnits, types: Map[TypeId, ModuleMember], typeScope: TypeScope)
//final case class TypedModuleMember
//final case class PonyClass(name: TypeId, formalArgs: FormalArgs, is:List[TypeClass], typeBody: TypeBody)
//
//final case class TypedModuleMember(name: TypeId, content: TypedBodyContent)
//final case class TypedDeclare(typeClass: TypeClass, is: Is, declareMap: Option[DeclareMap]) extends TypedBodyContent
//class TypedClass(val ponyClass: PonyParserClass, val symTable: VariableSymbolTable) extends TypedBodyContent

final case class PreTypedModule(imports: CompilationUnits, classes: Map[TypeId, ModuleMember])(implicit val typeScope: TypeScope)

final case class TypedModule(imports:CompilationUnits, classes: Map[TypeId, TModuleMember])(implicit val typeScope: TypeScope)

sealed abstract class TModuleMember(val name: TypeId)

final case class TPrimitive(typename: TypeId) extends TModuleMember(typename)
final case class TDeclare(typeClass: TTypeClass, is: TIs, declareMap: TDeclareMap) extends TModuleMember(typeClass.name)
final case class TType(n: TypeId, ofType: TOfType, is: Option[TIs]) extends TModuleMember(n)

abstract class PonyClass(val na: TypeId, val formalArgs: FormalArgs, val is:TIs, val typeBody: TTypeBody) extends TModuleMember(na)
final case class TActor(n: TypeId, f: FormalArgs, i:Option[TIs], t: TTypeBody)   extends PonyClass(n,f,i.getOrElse(new TIs(List.empty)),t)
final case class TTrait(n: TypeId, f: FormalArgs, i:Option[TIs], t: TTypeBody)   extends PonyClass(n,f,i.getOrElse(new TIs(List.empty)),t)
final case class TObject(n: TypeId, f: FormalArgs, i:Option[TIs], t: TTypeBody)  extends PonyClass(n,f,i.getOrElse(new TIs(List.empty)),t)

final case class TCombinedArgs(formalArgs: FormalArgs, args: TArgs)
final case class TArg(expr: Option[TExpr], ofType: Option[TOfType], assign: Option[TExpr])

sealed abstract class TTypeElement
final case class TPartialType(name: TTypeClass) extends TTypeElement
final case class TTypeClass(name: TypeId, module:Option[TypeId] = None, mode: Option[TMode] = None, formalArgs: FormalArgs = None) extends TTypeElement
final case class TLambda(mode: TMode, args: List[TArg], result: Option[List[TArg]], throws: Boolean, block: Option[TBlock]) extends TTypeElement

sealed abstract class TMode
object Immutable    extends TMode
object Mutable      extends TMode
object Unique       extends TMode
final case class TModeExpr(expr: TExpr) extends TMode

final case class TBlock(contents:List[TBlockContent], catchBlock: Option[TBlock], alwaysBlock: Option[TBlock]) extends TBlockContent
final case class TIs(list: List[TTypeClass])
final case class TDeclareMap(map: List[TPonyMap])
final case class TPonyMap(from:ID, to: ID)

final case class TOfType(typeList: List[TTypeElement])


final case class TTypeBody(body: Map[ID,TBodyContent])

sealed abstract class TBodyContent(val name: ID, val isAbstract: Boolean = false, val returnType: TOfType = new TOfType(List(new TTypeClass("Void"))))
final case class TField(id: ID, ofType: TOfType, expr: Option[TExpr]) extends TBodyContent(id, expr.isEmpty, ofType)
final case class TDelegate(id: ID, ofType: TOfType) extends TBodyContent(name = id, returnType = ofType)
final case class TConstructor(contents: TMethodContent, throws: Boolean, block: Option[TBlock]) extends TBodyContent(contents.id, block.isEmpty)
final case class TAmbient(contents: TMethodContent, throws: Boolean, block: Option[TBlock]) extends TBodyContent(contents.id, block.isEmpty)
final case class TFunction(contents: TMethodContent, results: Option[TArgs], throws: Boolean, block: Option[TBlock]) extends TBodyContent(contents.id, block.isEmpty)
final case class TMessage(contents: TMethodContent, block: Option[TBlock]) extends TBodyContent(contents.id, block.isEmpty)

final case class TMethodContent(mode: Option[TMode], id:ID, combinedArgs: TCombinedArgs)

final case class TExpr(unary: TUnary, operator: List[(Operator, TUnary)])

trait TBlockContent

object Return extends TBlockContent
object Throw extends TBlockContent
object Break extends TBlockContent
object Continue extends TBlockContent

final case class TVarDec(id: ID, ofType: Option[TOfType]) extends TBlockContent
final case class TMatch(list: List[TExpr], cases: List[TCaseBlock]) extends TBlockContent
final case class TDoLoop(block: TBlock, whileExpr: TExpr) extends TBlockContent
final case class TWhileLoop(whileExpr: TExpr, block: TBlock) extends TBlockContent
final case class TForLoop(forVars: List[TForVar], inExpr: TExpr, block: TBlock) extends TBlockContent
final case class TConditional(conditionalList: List[(TExpr, TBlock)], elseBlock: Option[TBlock]) extends TBlockContent
final case class TAssignment(lValues: List[TLValue], expr: Option[TExpr]) extends TBlockContent

final case class TCaseBlock(c: Option[TCaseSubBlock], block: TBlock)
abstract class TCaseSubBlock
final case class TCaseIf(expr: TExpr) extends TCaseSubBlock
final case class TCaseVarList(varList: List[TCaseVar]) extends TCaseSubBlock

final case class TCaseVar(expr: Option[TExpr], forVar: TForVar)
final case class TForVar(id: ID, ofType: Option[TOfType])


sealed abstract class TLValue
final case class TLValueVar(nVar: TVarDec) extends TLValue
final case class TLValueCommand(command: TCommand) extends TLValue

sealed abstract class TUnary(unaryOps: List[UnaryOp])
final case class TUnaryCommand(un: List[UnaryOp], command: TCommand) extends TUnary(un)
final case class TUnaryLambda(un: List[UnaryOp], lambda: TLambda) extends TUnary(un)

final case class TCommand(first: TFirstCommand, second: Option[TSecondCommand])
sealed abstract class TFirstCommand
final case class TCommandExpr(expr: TExpr) extends TFirstCommand
final case class TCommandArgs(args: List[TArg]) extends TFirstCommand
sealed abstract class TAtom extends TFirstCommand

sealed abstract class TSecondCommand
final case class TSecondCommandArgs(args: TArgs) extends TSecondCommand
final case class TCommandCall(id: ID, formalArgs: FormalArgs, args: Args) extends TSecondCommand

abstract class TopTypeException(message: String) extends Exception(message)
final class TraitNotFoundException(msg: String) extends TopTypeException(msg)
final class ClassExtendsNonTraitType(msg: String) extends TopTypeException(msg)
final class AbstractMethodNotImplemented(msg: String) extends TopTypeException(msg)
final class OverrideException(msg: String) extends TopTypeException(msg)
final class FormalArgsMismatch(msg: String) extends TopTypeException(msg)
final class ModuleNotFoundException(message: String) extends TopTypeException(message)
final class TypeNotFoundException(message: String) extends TopTypeException(message)
final class DuplicateTypeException(message: String) extends TopTypeException(message)