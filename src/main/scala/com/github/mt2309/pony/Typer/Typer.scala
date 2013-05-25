package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.Common._
import com.github.mt2309.pony.AST._

/**
 * User: mthorpe
 * Date: 30/04/2013
 * Time: 00:57
 */

//final case class TypedModule(imports: CompilationUnits, types: Map[TypeId, ModuleMember], typeScope: TypeScope)
//final case class TypedModuleMember
//final case class PonyClass(typeClass: TypeId, formalArgs: FormalArgs, is:List[TypeClass], typeBody: TypeBody)
//
//final case class TypedModuleMember(typeClass: TypeId, content: TypedBodyContent)
//final case class TypedDeclare(typeClass: TypeClass, is: Is, declareMap: Option[DeclareMap]) extends TypedBodyContent
//class TypedClass(val ponyClass: PonyParserClass, val symTable: VariableSymbolTable) extends TypedBodyContent

final case class Scope(typeScope: TypeScope, imports: CompilationUnits, varScope: VariableScope = Map.empty, filename: Filename) {
  def search(t: TypeClass): ModuleMember = {
    val i = imports.searchType(t)

    if (i.isDefined)
      i.get
    else typeScope.getOrElse(t.name, throw new TypeClassNotFoundException(t.toString))
  }

  def checkIsList(i: Is): Boolean = {
    for (mem <- i.list) {
      search(mem)
    }

    true
  }

  def findMethod(id: ID, of: OfType): BodyContent = {
    for (t <- of.typeList) {
      t match {
        case p: PartialType => search(p.typeClass)
        case t: TypeClass => search(t)
        case l: Lambda => throw new LambdaInMethCallException(s"Lambda found in method lookup for for id $id")
      }
    }

    ???
  }

  def search(t: TypeId): ModuleMember = typeScope.getOrElse(t, throw new TypeNotFoundException(t))

  def searchID(i: ID): TOfType = varScope.getOrElse(i, throw new VariableNotFoundException(i))
}

final case class PreTypedModule(imports: CompilationUnits, classes: Map[TypeId, ModuleMember])(implicit val scope: Scope)

final case class TypedModule(imports:CompilationUnits, classes: Map[TypeId, TModuleMember])(implicit val scope: Scope)

sealed abstract class TModuleMember(val name: TypeId)

final case class TPrimitive(typename: TypeId) extends TModuleMember(typename) with TTypeElement
final case class TDeclare(typeClass: TTypeClass, is: TIs, declareMap: TDeclareMap) extends TModuleMember(typeClass.name)
final case class TType(n: TypeId, ofType: TOfType, is: TIs) extends TModuleMember(n)

abstract class PonyClass(val na: TypeId, val formalArgs: TFormalArgs, val is:TIs, val typeBody: TTypeBody) extends TModuleMember(na)
final case class TActor(n: TypeId, f: TFormalArgs, i:TIs, t: TTypeBody)   extends PonyClass(n,f,i,t)
final case class TTrait(n: TypeId, f: TFormalArgs, i:TIs, t: TTypeBody)   extends PonyClass(n,f,i,t)
final case class TObject(n: TypeId, f: TFormalArgs, i:TIs, t: TTypeBody)  extends PonyClass(n,f,i,t)

final case class TCombinedArgs(formalArgs: TFormalArgs, args: TArgs)
final case class TArg(expr: Option[TExpr], ofType: Option[TOfType], assign: Option[TExpr])

sealed trait TTypeElement
final case class TPartialType(name: TTypeClass) extends TTypeElement
final case class TTypeClass(name: TypeId, module:Option[TypeId] = None, mode: TMode = TReadOnly, formalArgs: TFormalArgs = List.empty) extends TTypeElement
final case class TLambda(mode: TMode, args: List[TArg], result: Option[List[TArg]], throws: Boolean, block: Option[TBlock]) extends TTypeElement

sealed abstract class TMode
object TReadOnly    extends TMode
object TImmutable    extends TMode
object TMutable      extends TMode
object TUnique       extends TMode
final case class TModeExpr(expr: TExpr) extends TMode

final case class TBlock(contents:List[TBlockContent], catchBlock: Option[TBlock], alwaysBlock: Option[TBlock]) extends TBlockContent
final case class TIs(list: List[TTypeClass])
final case class TDeclareMap(map: List[TPonyMap])
final case class TPonyMap(from:BodyContent, to: ID)

final case class TOfType(typeList: Set[TTypeElement])


final case class TTypeBody(body: Map[ID,TBodyContent])

sealed abstract class TBodyContent(val name: ID, val isAbstract: Boolean = false, val returnType: TOfType = new TOfType(Set(new TTypeClass("Void"))))
final case class TField(id: ID, ofType: TOfType, expr: Option[TExpr]) extends TBodyContent(id, expr.isEmpty, ofType)
final case class TDelegate(id: ID, ofType: TOfType) extends TBodyContent(name = id, returnType = ofType)
final case class TConstructor(contents: TMethodContent, throws: Boolean, block: Option[TBlock]) extends TBodyContent(contents.id, block.isEmpty)
final case class TAmbient(contents: TMethodContent, throws: Boolean, block: Option[TBlock]) extends TBodyContent(contents.id, block.isEmpty)
final case class TFunction(contents: TMethodContent, results: Option[TArgs], throws: Boolean, block: Option[TBlock]) extends TBodyContent(contents.id, block.isEmpty)
final case class TMessage(contents: TMethodContent, block: Option[TBlock]) extends TBodyContent(contents.id, block.isEmpty)

final case class TMethodContent(mode: TMode, id:ID, combinedArgs: TCombinedArgs)

final case class TExpr(unary: TUnary, operator: List[(Operator, TUnary)]) {
  def extractOfType(implicit scope: Scope): TOfType = ???
}

trait TBlockContent

object TReturn extends TBlockContent
object TThrow extends TBlockContent
object TBreak extends TBlockContent
object TContinue extends TBlockContent

final case class TVarDec(id: ID, ofType: Option[TOfType], expr: Option[TExpr]) extends TBlockContent
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

sealed abstract class TFirstCommand {
  def extractOfType(implicit scope: Scope): TOfType = ???
}

final case class TCommandExpr(expr: TExpr) extends TFirstCommand
final case class TCommandArgs(args: List[TArg]) extends TFirstCommand
sealed abstract class TAtom extends TFirstCommand

object TThis extends TAtom
object TTrue extends TAtom
object TFalse extends TAtom
final case class TPonyInt(i: Int) extends TAtom
final case class TPonyDouble(d: Double) extends TAtom
final case class TPonyString(s: String) extends TAtom
final case class TPonyID(i: ID) extends TAtom
final case class TPonyTypeId(t: TypeId) extends TAtom


sealed abstract class TSecondCommand
final case class TSecondCommandArgs(args: TArgs) extends TSecondCommand
final case class TCommandCall(id: BodyContent, formalArgs: TFormalArgs, args: TArgs) extends TSecondCommand