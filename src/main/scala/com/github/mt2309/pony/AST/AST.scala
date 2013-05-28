package com.github.mt2309.pony.AST

import com.github.mt2309.pony.Common._

/**
 * User: mthorpe
 * Date: 26/04/2013
 * Time: 00:16
 */

final case class Module(imports:Set[Use], classes: Map[TypeId, ModuleMember]) extends NotNull

final case class Use(toType: Option[TypeId], importName: String) extends NotNull

sealed abstract class ModuleMember(val typeName: TypeId)(implicit val fileName: Filename) extends NotNull

final case class Primitive(name: TypeId) extends ModuleMember(name)("Primitive value") with NotNull
final case class Declare(name: TypeId, is: Is, declareMap: DeclareMap)(implicit val filename: Filename) extends ModuleMember(name) with NotNull
final case class Type(n: TypeId, ofType: OfType, is: Is)(implicit val filename: Filename) extends ModuleMember(n) with NotNull

sealed abstract class
PonyParserClass(val name: TypeId, val formalArgs: FormalArgs, val is:Is, val typeBody: TypeBody)(implicit val filename: Filename)
  extends ModuleMember(name) with NotNull

final case class Actor
(n: TypeId, f: FormalArgs, i:Is, t: TypeBody)(implicit override val filename: Filename)  extends PonyParserClass(n,f,i,t) with NotNull
final case class Trait
(n: TypeId, f: FormalArgs, i:Is, t: TypeBody)(implicit override val filename: Filename)  extends PonyParserClass(n,f,i,t) with NotNull
final case class Object
(n: TypeId, f: FormalArgs, i:Is, t: TypeBody)(implicit override val filename: Filename)  extends PonyParserClass(n,f,i,t) with NotNull

final case class Param(name: ID, ofType: OfType) extends NotNull

final case class CombinedArgs(formalArgs: FormalArgs, args: Params) extends NotNull
final case class Arg(expr: Option[Expr], ofType: Option[OfType], assign: Option[Expr]) extends NotNull

sealed abstract class TypeElement(implicit val fileName: Filename) extends NotNull
final case class PartialType(typeClass: TypeClass)(implicit override val fileName: Filename) extends TypeElement with NotNull

final case class TypeClass(name: TypeId,
                           module:Option[TypeId] = None,
                           mode: Mode = ReadOnly,
                           formalArgs: FormalArgs = List.empty)(implicit override val fileName: Filename) extends TypeElement with NotNull {
  override def toString: String = if (module.isDefined) name ++ module.get else name
}

final case class Lambda(mode: Mode, args: Args, result: Option[Params], throws: Boolean, block: Option[Block])(implicit override val fileName: Filename) extends TypeElement with NotNull

sealed abstract class Mode extends NotNull
object ReadOnly     extends Mode with NotNull
object Immutable    extends Mode with NotNull
object Mutable      extends Mode with NotNull
object Unique       extends Mode with NotNull
final case class ModeExpr(expr: Expr) extends Mode with NotNull

final case class Block(contents:List[BlockContent], catchBlock: Option[Block], alwaysBlock: Option[Block]) extends BlockContent with NotNull
final case class Is(list: List[TypeClass]) extends NotNull
final case class DeclareMap(map: List[PonyMap]) extends NotNull
final case class PonyMap(from:ID, to: ID) extends NotNull

final case class OfType(typeList: Set[TypeElement]) extends NotNull

final case class TypeBody(body: Map[ID,BodyContent]) extends NotNull

sealed abstract class BodyContent(val name: ID, val isAbstract: Boolean = false, val returnType: OfType = voidOfType) extends NotNull
final case class Field(id: ID, ofType: OfType, expr: Option[Expr]) extends BodyContent(id, expr.isEmpty, ofType) with NotNull
final case class Delegate(id: ID, ofType: OfType) extends BodyContent(name = id, returnType = ofType) with NotNull
final case class Constructor(contents: MethodContent, throws: Boolean, block: Option[Block]) extends BodyContent(contents.id, block.isEmpty) with NotNull
final case class Ambient(contents: MethodContent, throws: Boolean, block: Option[Block]) extends BodyContent(contents.id, block.isEmpty) with NotNull
final case class Function(contents: MethodContent, results: Option[Params], throws: Boolean, block: Option[Block]) extends BodyContent(contents.id, block.isEmpty) with NotNull
final case class Message(contents: MethodContent, block: Option[Block]) extends BodyContent(contents.id, block.isEmpty) with NotNull

final case class MethodContent(mode: Mode, id:ID, combinedArgs: CombinedArgs) extends NotNull

final case class Expr(unary: Unary, operator: List[(Operator, Unary)]) extends NotNull

sealed trait BlockContent extends NotNull

object Return extends BlockContent with NotNull
object Throw extends BlockContent with NotNull
object Break extends BlockContent with NotNull
object Continue extends BlockContent with NotNull

final case class VarDec(id: ID, ofType: Option[OfType], assign: Option[Expr]) extends BlockContent with NotNull
final case class Match(list: List[Expr], cases: List[CaseBlock]) extends BlockContent with NotNull
final case class DoLoop(block: Block, whileExpr: Expr) extends BlockContent with NotNull
final case class WhileLoop(whileExpr: Expr, block: Block) extends BlockContent with NotNull
final case class ForLoop(forVars: List[ForVar], inExpr: Expr, block: Block) extends BlockContent with NotNull
final case class Conditional(conditionalList: List[(Expr, Block)], elseBlock: Option[Block]) extends BlockContent with NotNull
final case class Assignment(lValues: List[LValue], expr: Option[Expr]) extends BlockContent with NotNull

final case class CaseBlock(subBlock: Option[CaseSubBlock], block: Block) extends NotNull
sealed abstract class CaseSubBlock extends NotNull
final case class CaseIf(expr: Expr) extends CaseSubBlock with NotNull
final case class CaseVarList(varList: List[CaseVar]) extends CaseSubBlock with NotNull

final case class CaseVar(expr: Option[Expr], forVar: ForVar) extends NotNull
final case class ForVar(id: ID, ofType: Option[OfType]) extends NotNull


sealed abstract class LValue extends NotNull
final case class LValueVar(nVar: VarDec) extends LValue with NotNull
final case class LValueCommand(command: Command) extends LValue with NotNull

// "+" | "-" | "*" | "/" | "%" | "<<" | ">>" | ">" | "<" | ">=" | "<=" | "!=" | "-" | "#=" | "~=" | "|" | "^" | "&"
sealed abstract class Operator extends NotNull
object PLUS extends Operator with NotNull
object MINUS extends Operator with NotNull
object TIMES extends Operator with NotNull
object DIVIDE extends Operator with NotNull
object MOD extends Operator with NotNull
object LSHIFT extends Operator with NotNull
object RSHIFT extends Operator with NotNull
object GT extends Operator with NotNull
object LT extends Operator with NotNull
object GTE extends Operator with NotNull
object LTE extends Operator with NotNull
object NE extends Operator with NotNull
object STEQ extends Operator with NotNull
object NSTEQ extends Operator with NotNull
object OR extends Operator with NotNull
object AND extends Operator with NotNull
object XOR extends Operator with NotNull

sealed abstract class Unary(unaryOps: List[UnaryOp]) extends NotNull
final case class UnaryCommand(un: List[UnaryOp], command: Command) extends Unary(un) with NotNull
final case class UnaryLambda(un: List[UnaryOp], lambda: Lambda) extends Unary(un) with NotNull

final case class Command(first: FirstCommand, second: Option[SecondCommand]) extends NotNull
sealed abstract class FirstCommand extends NotNull
final case class CommandExpr(expr: Expr) extends FirstCommand with NotNull
final case class CommandArgs(args: List[Arg]) extends FirstCommand with NotNull
sealed abstract class Atom extends FirstCommand with NotNull

sealed abstract class SecondCommand extends NotNull
final case class SecondCommandArgs(args: Args) extends SecondCommand with NotNull
final case class CommandCall(id: ID, formalArgs: FormalArgs, args: Args) extends SecondCommand with NotNull

object This extends Atom with NotNull
object True extends Atom with NotNull
object False extends Atom with NotNull
final case class PonyInt(i: Int) extends Atom with NotNull
final case class PonyDouble(d: Double) extends Atom with NotNull
final case class PonyString(s: String) extends Atom with NotNull
final case class PonyID(i: ID) extends Atom with NotNull
final case class PonyTypeId(t: TypeId) extends Atom with NotNull

sealed abstract class UnaryOp extends NotNull
object PARTIAL extends UnaryOp with NotNull
object UNARY_MINUS extends UnaryOp with NotNull
object UNARY_BANG extends UnaryOp with NotNull