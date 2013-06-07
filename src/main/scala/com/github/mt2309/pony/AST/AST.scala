package com.github.mt2309.pony.AST

import com.github.mt2309.pony.Common._
import scala.util.parsing.input.{Position, Positional}

import scala.language.implicitConversions

/**
 * User: mthorpe
 * Date: 26/04/2013
 * Time: 00:16
 */

trait PonyPos extends Positional

sealed trait AST extends NotNull with PonyPos {
  implicit def toPos: Position = this.pos
}

final case class Module(imports:Set[Use], classes: Map[TypeId, ModuleMember]) extends AST

final case class Use(toType: Option[TypeId], importName: String) extends AST

sealed abstract class ModuleMember(val typeName: TypeId)(implicit val fileName: Filename) extends AST

final case class Primitive(name: TypeId) extends ModuleMember(name)(primitiveFilename) with AST
final case class Declare(name: TypeId, is: Is, declareMap: DeclareMap)(implicit val filename: Filename) extends ModuleMember(name) with AST
final case class Type(n: TypeId, ofType: OfType, is: Is)(implicit val filename: Filename) extends ModuleMember(n) with AST

sealed abstract class
PonyParserClass(val name: TypeId, val formalArgs: FormalArgs, val is:Is, val typeBody: TypeBody)(implicit val filename: Filename)
  extends ModuleMember(name) with AST

final case class Actor
(n: TypeId, f: FormalArgs, i:Is, t: TypeBody)(implicit override val filename: Filename) extends PonyParserClass(n,f,i,t) with AST
final case class Trait
(n: TypeId, f: FormalArgs, i:Is, t: TypeBody)(implicit override val filename: Filename) extends PonyParserClass(n,f,i,t) with AST
final case class Object
(n: TypeId, f: FormalArgs, i:Is, t: TypeBody, isStatic: Boolean)(implicit override val filename: Filename) extends PonyParserClass(n,f,i,t) with AST

final case class Param(name: ID, ofType: OfType) extends AST

final case class CombinedArgs(formalArgs: FormalParams, args: Params) extends AST
final case class Arg(expr: Option[Expr], ofType: Option[OfType], assign: Option[Expr]) extends AST

sealed abstract class TypeElement(implicit val fileName: Filename) extends AST
final case class PartialType(typeClass: TypeClass)(implicit override val fileName: Filename) extends TypeElement with AST

final case class TypeClass(name: TypeId,
                           module:Option[TypeId] = None,
                           mode: Mode = ReadOnly,
                           formalArgs: FormalArgs = List.empty)(implicit override val fileName: Filename) extends TypeElement with AST {
  override def toString: String = if (module.isDefined) name ++ "::" ++ module.get else name
}

final case class Lambda(mode: Mode, args: Args, result: Params, throws: Boolean, block: Option[Block])(implicit override val fileName: Filename) extends TypeElement with AST

sealed abstract class Mode extends AST
object ReadOnly     extends Mode with AST
object Immutable    extends Mode with AST
object Mutable      extends Mode with AST
object Unique       extends Mode with AST
final case class ModeExpr(expr: Expr) extends Mode with AST

final case class Block(contents:List[BlockContent], catchBlock: Option[Block], alwaysBlock: Option[Block]) extends BlockContent with AST
final case class Is(list: List[TypeClass]) extends AST
final case class DeclareMap(map: List[PonyMap]) extends AST
final case class PonyMap(from:ID, to: ID) extends AST

final case class OfType(typeList: Set[TypeElement]) extends AST

final case class TypeBody(body: Map[ID,BodyContent]) extends AST

sealed abstract class BodyContent(val name: ID, val isAbstract: Boolean = false, val returnType: OfType = voidOfType) extends AST
final case class Field(id: ID, ofType: OfType, expr: Option[Expr]) extends BodyContent(id, expr.isEmpty, ofType) with AST
final case class Delegate(id: ID, ofType: OfType) extends BodyContent(name = id, returnType = ofType) with AST
final case class Constructor(contents: MethodContent, throws: Boolean, block: Option[Block]) extends BodyContent(contents.id, block.isEmpty) with AST
final case class Ambient(contents: MethodContent, throws: Boolean, block: Option[Block]) extends BodyContent(contents.id, block.isEmpty) with AST
final case class Function(contents: MethodContent, results: Params, throws: Boolean, block: Option[Block]) extends BodyContent(contents.id, block.isEmpty) with AST
final case class Message(contents: MethodContent, block: Option[Block]) extends BodyContent(contents.id, block.isEmpty) with AST

final case class MethodContent(mode: Mode, id:ID, combinedArgs: CombinedArgs) extends AST

final case class Expr(unary: Unary, operator: List[(Operator, Unary)]) extends AST

sealed trait BlockContent extends AST

object Return extends BlockContent with AST
object Throw extends BlockContent with AST
object Break extends BlockContent with AST
object Continue extends BlockContent with AST

final case class VarDec(id: ID, ofType: Option[OfType], assign: Option[Expr]) extends BlockContent with AST
final case class Match(list: List[Expr], cases: List[CaseBlock]) extends BlockContent with AST
final case class DoLoop(block: Block, whileExpr: Expr) extends BlockContent with AST
final case class WhileLoop(whileExpr: Expr, block: Block) extends BlockContent with AST
final case class ForLoop(forVars: List[ForVar], inExpr: Expr, block: Block) extends BlockContent with AST
final case class Conditional(conditionalList: List[(Expr, Block)], elseBlock: Option[Block]) extends BlockContent with AST
final case class Assignment(lValues: List[LValue], expr: Option[Expr]) extends BlockContent with AST

final case class CaseBlock(subBlock: Option[CaseSubBlock], block: Block) extends AST
sealed abstract class CaseSubBlock extends AST
final case class CaseIf(expr: Expr) extends CaseSubBlock with AST
final case class CaseVarList(varList: List[CaseVar]) extends CaseSubBlock with AST

final case class CaseVar(expr: Option[Expr], forVar: ForVar) extends AST
final case class ForVar(id: ID, ofType: Option[OfType]) extends AST


sealed abstract class LValue extends AST
final case class LValueVar(nVar: VarDec) extends LValue with AST
final case class LValueCommand(command: Command) extends LValue with AST

// "+" | "-" | "*" | "/" | "%" | "<<" | ">>" | ">" | "<" | ">=" | "<=" | "!=" | "-" | "#=" | "~=" | "|" | "^" | "&"
sealed abstract class Operator extends AST
sealed abstract class NumericOp extends Operator
sealed abstract class BooleanOp extends Operator
sealed abstract class NumericBooleanOp extends Operator
sealed abstract class TypeOp extends Operator

final class Plus    extends NumericOp with AST
final class Minus   extends NumericOp with AST
final class Times   extends NumericOp with AST
final class Divide  extends NumericOp with AST
final class Mod     extends NumericOp with AST
final class LShift  extends NumericOp with AST
final class RShift  extends NumericOp with AST
final class GT      extends BooleanOp with AST
final class LT      extends BooleanOp with AST
final class GTE     extends BooleanOp with AST
final class LTE     extends BooleanOp with AST
final class STEq    extends TypeOp with AST
final class STNeq   extends TypeOp with AST
final class EQ      extends NumericBooleanOp with AST
final class NE      extends NumericBooleanOp with AST
final class Or      extends NumericBooleanOp with AST
final class And     extends NumericBooleanOp with AST
final class XOr     extends NumericBooleanOp with AST

sealed abstract class Unary(unaryOps: List[UnaryOp]) extends AST
final case class UnaryCommand(un: List[UnaryOp], command: Command) extends Unary(un) with AST
final case class UnaryLambda(un: List[UnaryOp], lambda: Lambda) extends Unary(un) with AST

final case class Command(first: FirstCommand, second: Option[SecondCommand]) extends AST
sealed abstract class FirstCommand extends AST
final case class CommandExpr(expr: Expr) extends FirstCommand with AST
final case class CommandArgs(args: List[Arg]) extends FirstCommand with AST
sealed abstract class Atom extends FirstCommand with AST

sealed abstract class SecondCommand extends AST
final case class SecondCommandArgs(args: Args) extends SecondCommand with AST
final case class CommandCall(id: ID, formalArgs: FormalArgs, args: Args) extends SecondCommand with AST

object This extends Atom with AST
object True extends Atom with AST
object False extends Atom with AST
final case class PonyInt(i: Int) extends Atom with AST
final case class PonyDouble(d: Double) extends Atom with AST
final case class PonyString(s: String) extends Atom with AST
final case class PonyID(i: ID) extends Atom with AST
final case class PonyTypeId(t: TypeId) extends Atom with AST

sealed abstract class UnaryOp extends AST
object PARTIAL extends UnaryOp with AST
object UNARY_MINUS extends UnaryOp with AST
object UNARY_BANG extends UnaryOp with AST
