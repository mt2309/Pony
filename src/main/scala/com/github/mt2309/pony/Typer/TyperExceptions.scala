package com.github.mt2309.pony.Typer

/**
 * User: mthorpe
 * Date: 23/05/2013
 * Time: 18:20
 */
abstract class TopTypeException(message: String) extends Exception(message)
final class TraitNotFoundException(msg: String) extends TopTypeException(msg)
final class ClassExtendsNonTraitType(msg: String) extends TopTypeException(msg)
final class AbstractMethodNotImplemented(msg: String) extends TopTypeException(msg)
final class OverrideException(msg: String) extends TopTypeException(msg)
final class FormalArgsMismatch(msg: String) extends TopTypeException(msg)
final class ModuleNotFoundException(message: String) extends TopTypeException(message)
final class TypeNotFoundException(message: String) extends TopTypeException(message)
final class DuplicateTypeException(message: String) extends TopTypeException(message)

abstract class TyperException(message: String) extends Exception(message)
final class PrimitiveFound(msg: String) extends TyperException(msg)
final class TypeClassNotFoundException(msg: String) extends TyperException(msg)
final class VariableNotFoundException(msg: String) extends TyperException(msg)
final class AssignmentException(msg: String) extends TyperException(msg)
final class VariableShadowingException(msg: String) extends TyperException(msg)
final class LambdaInMethCallException(msg: String) extends TyperException(msg)
final class TypeMismatch(expected: String, got: String) extends TyperException(s"Got $expected, but expected $got")

final class TyperInferenceException extends TyperException("Type inference is not enabled or working yet, please annotate with types")