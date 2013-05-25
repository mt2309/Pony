package com.github.mt2309.pony.AST

import com.github.mt2309.pony.Common._

/**
 * User: mthorpe
 * Date: 26/04/2013
 * Time: 00:16
 */

final case class Module(imports:Set[Use], classes: Map[TypeId, ModuleMember])

final case class Use(toType: Option[TypeId], importName: String)

sealed abstract class ModuleMember(val typeName: TypeId)

final case class Primitive(name: TypeId) extends ModuleMember(name)
final case class Declare(typeClass: TypeClass, is: Is, declareMap: DeclareMap) extends ModuleMember(typeClass.name)
final case class Type(n: TypeId, ofType: OfType, is: Is) extends ModuleMember(n)

sealed abstract class
PonyParserClass(val name: TypeId, val formalArgs: FormalArgs, val is:Is, val typeBody: TypeBody)
  extends ModuleMember(name)

final case class Actor(n: TypeId, f: FormalArgs, i:Is, t: TypeBody)   extends PonyParserClass(n,f,i,t)
final case class Trait(n: TypeId, f: FormalArgs, i:Is, t: TypeBody)   extends PonyParserClass(n,f,i,t)
final case class Object(n: TypeId, f: FormalArgs, i:Is, t: TypeBody)  extends PonyParserClass(n,f,i,t)

final case class CombinedArgs(formalArgs: FormalArgs, args: Args)
final case class Arg(expr: Option[Expr], ofType: Option[OfType], assign: Option[Expr])

sealed abstract class TypeElement
final case class PartialType(typeClass: TypeClass) extends TypeElement
final case class TypeClass(name: TypeId, module:Option[TypeId] = None, mode: Mode = ReadOnly, formalArgs: FormalArgs = List.empty) extends TypeElement {
  override def toString: String = if (module.isDefined) name ++ module.get else name
}
final case class Lambda(mode: Mode, args: List[Arg], result: Option[List[Arg]], throws: Boolean, block: Option[Block]) extends TypeElement

sealed abstract class Mode
object ReadOnly     extends Mode
object Immutable    extends Mode
object Mutable      extends Mode
object Unique       extends Mode
final case class ModeExpr(expr: Expr) extends Mode

final case class Block(contents:List[BlockContent], catchBlock: Option[Block], alwaysBlock: Option[Block]) extends BlockContent
final case class Is(list: List[TypeClass])
final case class DeclareMap(map: List[PonyMap])
final case class PonyMap(from:ID, to: ID)

final case class OfType(typeList: Set[TypeElement])


final case class TypeBody(body: Map[ID,BodyContent])

sealed abstract class BodyContent(val name: ID, val isAbstract: Boolean = false, val returnType: OfType = new OfType(Set(new TypeClass("Void"))))
final case class Field(id: ID, ofType: OfType, expr: Option[Expr]) extends BodyContent(id, expr.isEmpty, ofType)
final case class Delegate(id: ID, ofType: OfType) extends BodyContent(name = id, returnType = ofType)
final case class Constructor(contents: MethodContent, throws: Boolean, block: Option[Block]) extends BodyContent(contents.id, block.isEmpty)
final case class Ambient(contents: MethodContent, throws: Boolean, block: Option[Block]) extends BodyContent(contents.id, block.isEmpty)
final case class Function(contents: MethodContent, results: Option[Args], throws: Boolean, block: Option[Block]) extends BodyContent(contents.id, block.isEmpty)
final case class Message(contents: MethodContent, block: Option[Block]) extends BodyContent(contents.id, block.isEmpty)

final case class MethodContent(mode: Mode, id:ID, combinedArgs: CombinedArgs)

final case class Expr(unary: Unary, operator: List[(Operator, Unary)])

sealed trait BlockContent

object Return extends BlockContent
object Throw extends BlockContent
object Break extends BlockContent
object Continue extends BlockContent

final case class VarDec(id: ID, ofType: Option[OfType], assign: Option[Expr]) extends BlockContent
final case class Match(list: List[Expr], cases: List[CaseBlock]) extends BlockContent
final case class DoLoop(block: Block, whileExpr: Expr) extends BlockContent
final case class WhileLoop(whileExpr: Expr, block: Block) extends BlockContent
final case class ForLoop(forVars: List[ForVar], inExpr: Expr, block: Block) extends BlockContent
final case class Conditional(conditionalList: List[(Expr, Block)], elseBlock: Option[Block]) extends BlockContent
final case class Assignment(lValues: List[LValue], expr: Option[Expr]) extends BlockContent

final case class CaseBlock(c: Option[CaseSubBlock], block: Block)
sealed abstract class CaseSubBlock
final case class CaseIf(expr: Expr) extends CaseSubBlock
final case class CaseVarList(varList: List[CaseVar]) extends CaseSubBlock

final case class CaseVar(expr: Option[Expr], forVar: ForVar)
final case class ForVar(id: ID, ofType: Option[OfType])


sealed abstract class LValue
final case class LValueVar(nVar: VarDec) extends LValue
final case class LValueCommand(command: Command) extends LValue

// "+" | "-" | "*" | "/" | "%" | "<<" | ">>" | ">" | "<" | ">=" | "<=" | "!=" | "-" | "#=" | "~=" | "|" | "^" | "&"
sealed abstract class Operator
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

sealed abstract class Unary(unaryOps: List[UnaryOp])
final case class UnaryCommand(un: List[UnaryOp], command: Command) extends Unary(un)
final case class UnaryLambda(un: List[UnaryOp], lambda: Lambda) extends Unary(un)

final case class Command(first: FirstCommand, second: Option[SecondCommand])
sealed abstract class FirstCommand
final case class CommandExpr(expr: Expr) extends FirstCommand
final case class CommandArgs(args: List[Arg]) extends FirstCommand
sealed abstract class Atom extends FirstCommand

sealed abstract class SecondCommand
final case class SecondCommandArgs(args: Args) extends SecondCommand
final case class CommandCall(id: ID, formalArgs: FormalArgs, args: Args) extends SecondCommand

object This extends Atom
object True extends Atom
object False extends Atom
final case class PonyInt(i: Int) extends Atom
final case class PonyDouble(d: Double) extends Atom
final case class PonyString(s: String) extends Atom
final case class PonyID(i: ID) extends Atom
final case class PonyTypeId(t: TypeId) extends Atom

sealed abstract class UnaryOp
object PARTIAL extends UnaryOp
object UNARY_MINUS extends UnaryOp
object UNARY_BANG extends UnaryOp