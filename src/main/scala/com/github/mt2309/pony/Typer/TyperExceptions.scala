package com.github.mt2309.pony.Typer

import scala.util.parsing.input.Position
import com.github.mt2309.pony.AST.BodyContent
import com.github.mt2309.pony.Common.{ID, TypeId}

/**
 * User: mthorpe
 * Date: 23/05/2013
 * Time: 18:20
 */
abstract class TopTypeException(message: String)(implicit pos: Position) extends Exception(s"$message\tat\t${pos.toString}")
private final class TraitNotFoundException(msg: String)(implicit pos: Position) extends TopTypeException(msg)
private final class ClassExtendsNonTraitType(msg: String)(implicit pos: Position) extends TopTypeException(msg)
private final class AbstractMethodNotImplemented(msg: String)(implicit pos: Position) extends TopTypeException(msg)
private final class OverrideException(msg: String)(implicit pos: Position) extends TopTypeException(msg)
private final class FormalArgsMismatch(msg: String)(implicit pos: Position) extends TopTypeException(msg)
final class ModuleNotFoundException(message: String)(implicit pos: Position) extends TopTypeException(message)
private final class TypeNotFoundException(message: String)(implicit pos: Position) extends TopTypeException(message)
private final class DuplicateTypeException(message: String)(implicit pos: Position) extends TopTypeException(message)

abstract class TyperException(message: String)(implicit pos: Position, scope: Scope) extends Exception(s"$message at $pos in ${scope.filename}")
private final class PrimitiveFound(msg: String)(implicit pos: Position, scope: Scope) extends TyperException(msg)
private final class EmptyTypeFound(msg: String)(implicit pos: Position, scope: Scope) extends TyperException(msg)
private final class ThisTypeOutsideClass(implicit pos: Position) extends TopTypeException("this.type found outside of a class")

private final class TypeClassNotFoundException(msg: String)(implicit pos: Position, scope: Scope) extends TyperException(msg)
private final class VariableNotFoundException(msg: String)(implicit pos: Position, scope: Scope) extends TyperException(msg)
private final class MethodNotFoundException(id: ID, name: TypeId)(implicit pos: Position, scope: Scope) extends TyperException(s"Could not find method $id in class $name")

private final class VariableOutOfScope(id: ID)(implicit pos: Position, scope: Scope) extends TyperException(s"Variable $id is out of scope, are you reusing a unique?")

private final class AssignmentException(msg: String)(implicit pos: Position, scope: Scope) extends TyperException(msg)
private final class VariableShadowingException(msg: String)(implicit pos: Position, scope: Scope) extends TyperException(msg)
private final class FieldDefinedInTrait(msg: String)(implicit pos: Position, scope: Scope) extends TyperException(msg)
private final class TypeShadowingException(msg: String)(implicit pos: Position, scope: Scope) extends TyperException(msg)
private final class LambdaInMethCallException(msg: String)(implicit pos: Position, scope: Scope) extends TyperException(msg)
private final class TypeMismatch(expected: String, got: String)(implicit pos: Position, scope: Scope) extends TyperException(s"Got $got, but expected $expected")
private final class ThisUsedOutsideClassException(implicit pos: Position, scope: Scope) extends TyperException("This was used outside of a class")
private final class UntypedListException(msg: ID)(implicit pos: Position, scope: Scope) extends TyperException(s"Variable $msg has no type associated with it")

private final class ArgumentMismatchException(b: TBodyContent)(implicit pos: Position, scope: Scope) extends TyperException(s"Argument length mismatch on body of name ${b.name}")


private final class TyperInferenceException(pos: Position, scope: Scope) extends TyperException("Type inference is not enabled or working yet, please annotate with types")(pos, scope)
