package com.github.mt2309.pony.Typer

import scala.util.parsing.input.Position

/**
 * User: mthorpe
 * Date: 23/05/2013
 * Time: 18:20
 */
abstract class TopTypeException(message: String)(implicit pos: Position) extends Exception(s"$message\tat\t${pos.toString}")
final class TraitNotFoundException(msg: String)(implicit pos: Position) extends TopTypeException(msg)
final class ClassExtendsNonTraitType(msg: String)(implicit pos: Position) extends TopTypeException(msg)
final class AbstractMethodNotImplemented(msg: String)(implicit pos: Position) extends TopTypeException(msg)
final class OverrideException(msg: String)(implicit pos: Position) extends TopTypeException(msg)
final class FormalArgsMismatch(msg: String)(implicit pos: Position) extends TopTypeException(msg)
final class ModuleNotFoundException(message: String)(implicit pos: Position) extends TopTypeException(message)
final class TypeNotFoundException(message: String)(implicit pos: Position) extends TopTypeException(message)
final class DuplicateTypeException(message: String)(implicit pos: Position) extends TopTypeException(message)

abstract class TyperException(message: String)(implicit pos: Position, scope: Scope) extends Exception(s"$message at $pos in ${scope.filename}")
final class PrimitiveFound(msg: String)(implicit pos: Position, scope: Scope) extends TyperException(msg)
final class EmptyTypeFound(msg: String)(implicit pos: Position, scope: Scope) extends TyperException(msg)
final class TypeClassNotFoundException(msg: String)(implicit pos: Position, scope: Scope) extends TyperException(msg)
final class VariableNotFoundException(msg: String)(implicit pos: Position, scope: Scope) extends TyperException(msg)
final class AssignmentException(msg: String)(implicit pos: Position, scope: Scope) extends TyperException(msg)
final class VariableShadowingException(msg: String)(implicit pos: Position, scope: Scope) extends TyperException(msg)
final class TypeShadowingException(msg: String)(implicit pos: Position, scope: Scope) extends TyperException(msg)
final class LambdaInMethCallException(msg: String)(implicit pos: Position, scope: Scope) extends TyperException(msg)
final class TypeMismatch(expected: String, got: String)(implicit pos: Position, scope: Scope) extends TyperException(s"Got $expected, but expected $got")
final class ThisUsedOutsideClassException(implicit pos: Position, scope: Scope) extends TyperException("This was used outside of a class")


final class TyperInferenceException(implicit pos: Position, scope: Scope) extends TyperException("Type inference is not enabled or working yet, please annotate with types")