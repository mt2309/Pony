package com.github.mt2309.pony.AST

/**
 * User: mthorpe
 * Date: 26/04/2013
 * Time: 00:16
 */

sealed abstract class AST

final case class Module(members:List[ClassMembers]) extends AST

abstract class ClassMembers extends AST

final case class Use(toType: Option[TypeId], importName: String) extends ClassMembers
final case class Declare(typeClass: TypeClass, is: Option[Is], declareMap: DeclareMap) extends ClassMembers
final case class Type(name: TypeId, ofType: OfType, is: Option[Is]) extends ClassMembers

final case class Actor(name: TypeId, formalArgs: FormalArgs, is:Option[Is], typeBody: TypeBody) extends ClassMembers
final case class Trait(name: TypeId, formalArgs: FormalArgs, is:Option[Is], typeBody: TypeBody) extends ClassMembers
final case class Object(name: TypeId, formalArgs: FormalArgs, is:Option[Is], typeBody: TypeBody) extends ClassMembers

final case class CombinedArgs(formalArgs: FormalArgs, Args: Args)

abstract class TypeElement extends AST
final case class PartialType(name: TypeClass) extends TypeElement
final case class TypeClass(name: TypeId, module:Option[TypeId], mode: Option[Mode], formalArgs: FormalArgs) extends TypeElement
final case class Lambda(mode: Mode, args: List[Arg], result: Option[List[Arg]], throws: Boolean, block: Option[Block]) extends TypeElement

abstract class Mode extends AST
object Immutable    extends Mode
object Mutable      extends Mode
object Unique       extends Mode
final case class ModeExpr(expr: Expr) extends Mode

final case class Block(contents:List[BlockContent], catchBlock: Option[Block], alwaysBlock: Option[Block]) extends AST with BlockContent
final case class Is(list: List[TypeClass]) extends AST
final case class DeclareMap(map: List[PonyMap]) extends AST
final case class PonyMap(from:ID, to: ID) extends AST

final case class OfType(typeList: Option[List[TypeElement]]) extends AST

final case class Arg(expr: Expr, ofType: OfType, assign: Option[Expr]) extends AST

final case class TypeBody(body: List[BodyContent]) extends AST
abstract class BodyContent(name: ID) extends AST
final case class Field(id: ID, ofType: OfType, expr: Option[Expr]) extends BodyContent(id)
final case class Delegate(id: ID, ofType: OfType) extends BodyContent(id)

final case class MethodContent(mode: Option[Mode], id:ID, combinedArgs: CombinedArgs) extends AST
final case class Constructor(contents: MethodContent, throws: Boolean, block: Option[Block]) extends BodyContent(contents.id)
final case class Ambient(contents: MethodContent, throws: Boolean, block: Option[Block]) extends BodyContent(contents.id)
final case class Function(contents: MethodContent, results: Option[Args], throws: Boolean, block: Option[Block]) extends BodyContent(contents.id)
final case class Message(contents: MethodContent, block: Option[Block]) extends BodyContent(contents.id)

final case class Expr(unary: Unary, operator: Option[(Operator, Unary)]) extends AST

trait BlockContent extends AST

object Return extends BlockContent
object Throw extends BlockContent
object Break extends BlockContent
object Continue extends BlockContent

final case class VarDec(id: ID, ofType: OfType) extends AST
final case class Match(list: List[Expr], cases: List[CaseBlock]) extends BlockContent
final case class DoLoop(block: Block, whileExpr: Expr) extends BlockContent
final case class WhileLoop(whileExpr: Expr, block: Block) extends BlockContent
final case class ForLoop(forVars: List[ForVar], inExpr: Expr, block: Block) extends BlockContent
final case class Conditional(conditionalList: List[(Expr, Block)], elseBlock: Option[Block]) extends BlockContent
final case class Assignment(lValues: List[LValue], expr: Option[Expr]) extends BlockContent

final case class CaseBlock(c: Option[Any], block: Block) extends AST
final case class CaseVar(expr: Option[Expr], forVar: ForVar) extends AST
final case class ForVar(id: ID, ofType: OfType) extends AST


abstract class LValue extends AST
final case class LValueVar(nVar: VarDec) extends LValue
final case class LValueCommand(command: Command) extends LValue

// "+" | "-" | "*" | "/" | "%" | "<<" | ">>" | ">" | "<" | ">=" | "<=" | "!=" | "-" | "#=" | "~=" | "|" | "^" | "&"
abstract class Operator extends AST
object PLUS extends Operator
object MINUS extends Operator
object TIMES extends Operator
object DIVIDE extends Operator
object MOD extends Operator
object LSHIFT extends Operator
object RSHIFT extends Operator
object GT extends Operator
object LT extends Operator
object GTE extends Operator
object LTE extends Operator
object NE extends Operator
object STEQ extends Operator
object NSTEQ extends Operator
object OR extends Operator
object AND extends Operator
object XOR extends Operator

abstract class Unary(unaryOps: List[UnaryOp]) extends AST
final case class UnaryCommand(un: List[UnaryOp], command: Command) extends Unary(un)
final case class UnaryLambda(un: List[UnaryOp], lambda: Lambda) extends Unary(un)

final case class Command(first: FirstCommand, second: SecondCommand) extends AST
abstract class FirstCommand extends AST
final case class CommandExpr(expr: Expr) extends FirstCommand
final case class CommandArgs(args: List[Arg]) extends FirstCommand
abstract class Atom extends FirstCommand

abstract class SecondCommand extends AST
final case class SecondCommandArgs(args: Args) extends SecondCommand
final case class CommandCall(id: ID, formalArgs: FormalArgs, args: Args) extends SecondCommand

object This extends Atom
object True extends Atom
object False extends Atom
final case class PonyInt(i: Int) extends Atom
final case class PonyString(s: String) extends Atom
final case class PonyID(i: ID) extends Atom
final case class PonyTypeId(t: TypeId) extends Atom

abstract class UnaryOp extends AST
object PARTIAL extends UnaryOp
object UNARY_MINUS extends UnaryOp
object UNARY_BANG extends UnaryOp