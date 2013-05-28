package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.Common._
import com.github.mt2309.pony.AST._
import com.github.mt2309.pony.CompilationUnit.{UnqualifiedCompilationUnits, QualifiedCompilationUnits}

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

final case class Scope(typeScope: TypeScope = Map.empty,
                       imports: CompilationUnits = (new QualifiedCompilationUnits(Map.empty) -> new UnqualifiedCompilationUnits(Set.empty)),
                       varScope: VariableScope = Map.empty,
                       filename: Filename = "Primitive") extends NotNull
{
  def search(t: TypeClass): IModuleMember = {
    val i = imports.searchType(t)

    i.getOrElse(typeScope.getOrElse(t.name, throw new TypeClassNotFoundException(t.toString)))
  }

  def search(m: ModuleMember): IModuleMember = search(m.typeName)

  def checkIsList(i: IIs): Boolean = {
    for (mem <- i.list) {
      search(mem.iType.name)
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

  def search(t: TypeId): IModuleMember = typeScope.getOrElse(t, throw new TypeNotFoundException(t))

  def searchID(i: ID): TOfType = varScope.getOrElse(i, throw new VariableNotFoundException(i))
}

final case class PreTypedModule(imports: CompilationUnits, classes: Map[TypeId, ModuleMember])(implicit val scope: Map[TypeId, ModuleMember], val filename: Filename) extends NotNull

final case class TypedModule(imports:CompilationUnits, classes: Map[TypeId, TModuleMember])(implicit val scope: Scope) extends NotNull

sealed abstract class TModuleMember(val name: TypeId)(implicit val scope: Scope) extends NotNull

final case class TPrimitive(typename: TypeId)(implicit override val scope: Scope) extends TModuleMember(typename) with TTypeElement
final case class TDeclare(typename: TypeId, is: TIs, declareMap: TDeclareMap)(implicit override val scope: Scope) extends TModuleMember(typename)
final case class TType(n: TypeId, ofType: TOfType, is: TIs)(implicit override val scope: Scope) extends TModuleMember(n)

abstract class PonyClass(val na: TypeId, val formalArgs: TFormalArgs, val is:TIs, val typeBody: TTypeBody)(implicit override val scope: Scope) extends TModuleMember(na)
final case class TActor(n: TypeId, f: TFormalArgs, i:TIs, t: TTypeBody)(implicit override val scope: Scope)   extends PonyClass(n,f,i,t)
final case class TTrait(n: TypeId, f: TFormalArgs, i:TIs, t: TTypeBody)(implicit override val scope: Scope)   extends PonyClass(n,f,i,t)
final case class TObject(n: TypeId, f: TFormalArgs, i:TIs, t: TTypeBody)(implicit override val scope: Scope)  extends PonyClass(n,f,i,t)

final case class TCombinedArgs(formalArgs: TFormalArgs, args: TArgs)(implicit val scope: Scope) extends NotNull
final case class TArg(expr: Option[TExpr], ofType: Option[TOfType], assign: Option[TExpr])(implicit val scope: Scope) extends NotNull

sealed trait TTypeElement extends NotNull
final case class TPartialType(name: TTypeClass)(implicit val scope: Scope) extends TTypeElement
final case class TTypeClass(moduleMember: IModuleMember, mode: TMode = TReadOnly, formalArgs: TFormalArgs = List.empty)(implicit val scope: Scope) extends TTypeElement
final case class TLambda(mode: TMode, args: List[TArg], result: Option[List[TArg]], throws: Boolean, block: Option[TBlock])(implicit val scope: Scope) extends TTypeElement

sealed abstract class TMode extends NotNull
object TReadOnly    extends TMode
object TImmutable    extends TMode
object TMutable      extends TMode
object TUnique       extends TMode
final case class TModeExpr(expr: TExpr) extends TMode

final case class TBlock(contents:List[TBlockContent], catchBlock: Option[TBlock], alwaysBlock: Option[TBlock])(implicit val scope: Scope) extends TBlockContent with NotNull
final case class TIs(list: List[TTypeClass])(implicit val scope: Scope) extends NotNull
final case class TDeclareMap(map: List[TPonyMap])(implicit val scope: Scope) extends NotNull
final case class TPonyMap(from:BodyContent, to: ID)(implicit val scope: Scope) extends NotNull

final case class TOfType(typeList: Set[TTypeElement])(implicit val scope: Scope) extends NotNull


final case class TTypeBody(body: Map[ID,TBodyContent])(implicit val scope: Scope) extends NotNull

sealed abstract class TBodyContent(val name: ID, val isAbstract: Boolean = false)(implicit val scope: Scope) extends NotNull
final case class TField(id: ID, ofType: TOfType, expr: Option[TExpr])(implicit override val scope: Scope) extends TBodyContent(id, expr.isEmpty)
final case class TDelegate(id: ID, ofType: TOfType)(implicit override val scope: Scope) extends TBodyContent(name = id)
final case class TConstructor(contents: TMethodContent, throws: Boolean, block: Option[TBlock])(implicit override val scope: Scope) extends TBodyContent(contents.id, block.isEmpty)
final case class TAmbient(contents: TMethodContent, throws: Boolean, block: Option[TBlock])(implicit override val scope: Scope) extends TBodyContent(contents.id, block.isEmpty)
final case class TFunction(contents: TMethodContent, results: Option[TArgs], throws: Boolean, block: Option[TBlock])(implicit override val scope: Scope) extends TBodyContent(contents.id, block.isEmpty)
final case class TMessage(contents: TMethodContent, block: Option[TBlock])(implicit override val scope: Scope) extends TBodyContent(contents.id, block.isEmpty)

final case class TMethodContent(mode: TMode, id:ID, combinedArgs: TCombinedArgs)(implicit val scope: Scope) extends NotNull

final case class TExpr(unary: TUnary, operator: List[(Operator, TUnary)])(implicit val scope: Scope) extends NotNull {
  def extractOfType(implicit scope: Scope): TOfType = ???
}

trait TBlockContent extends NotNull

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

final case class TCaseBlock(c: Option[TCaseSubBlock], block: TBlock)(implicit val scope: Scope) extends NotNull
abstract class TCaseSubBlock(implicit val scope: Scope) extends NotNull
final case class TCaseIf(expr: TExpr)(implicit override val scope: Scope) extends TCaseSubBlock
final case class TCaseVarList(varList: List[TCaseVar])(implicit override val scope: Scope) extends TCaseSubBlock

final case class TCaseVar(expr: Option[TExpr], forVar: TForVar)(implicit val scope: Scope) extends NotNull
final case class TForVar(id: ID, ofType: TOfType)(implicit val scope: Scope) extends NotNull


sealed abstract class TLValue(implicit val scope: Scope) extends NotNull
final case class TLValueVar(nVar: TVarDec)(implicit override val scope: Scope) extends TLValue
final case class TLValueCommand(command: TCommand)(implicit override val scope: Scope) extends TLValue

sealed abstract class TUnary(unaryOps: List[UnaryOp])(implicit val scope: Scope) extends NotNull
final case class TUnaryCommand(un: List[UnaryOp], command: TCommand)(implicit override val scope: Scope) extends TUnary(un)
final case class TUnaryLambda(un: List[UnaryOp], lambda: TLambda)(implicit override val scope: Scope) extends TUnary(un)

final case class TCommand(first: TFirstCommand, second: Option[TSecondCommand])(implicit val scope: Scope) extends NotNull

sealed abstract class TFirstCommand extends NotNull {
  def extractOfType(implicit scope: Scope): TOfType = ???
}

final case class TCommandExpr(expr: TExpr)(implicit val scope: Scope) extends TFirstCommand with NotNull
final case class TCommandArgs(args: List[TArg])(implicit val scope: Scope) extends TFirstCommand with NotNull
sealed abstract class TAtom extends TFirstCommand with NotNull

object TThis extends TAtom
object TTrue extends TAtom
object TFalse extends TAtom
final case class TPonyInt(i: Int) extends TAtom with NotNull
final case class TPonyDouble(d: Double) extends TAtom with NotNull
final case class TPonyString(s: String) extends TAtom with NotNull
final case class TPonyID(i: ID) extends TAtom with NotNull
final case class TPonyTypeId(t: TypeId) extends TAtom with NotNull


sealed abstract class TSecondCommand extends NotNull
final case class TSecondCommandArgs(args: TArgs) extends TSecondCommand with NotNull
final case class TCommandCall(id: BodyContent, formalArgs: TFormalArgs, args: TArgs) extends TSecondCommand with NotNull