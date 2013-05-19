package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.AST._
import com.github.mt2309.pony.Common._
import com.github.mt2309.pony.AST.TypeBody

/**
 * User: mthorpe
 * Date: 30/04/2013
 * Time: 00:57
 */

final case class TypedModule(imports: CompilationUnits, types: Map[TypeId, ModuleMember])
final case class PonyClass(name: TypeId, formalArgs: FormalArgs, is:List[TypeClass], typeBody: TypeBody)

final case class TypedModuleWithST(imports: CompilationUnits, types: Map[TypeId, TypedModuleMember])

final case class TypedModuleMember(name: TypeId, content: TypedBodyContent)

abstract class TypedBodyContent
final case class TypedDeclare(typeClass: TypeClass, is: Is, declareMap: Option[DeclareMap]) extends TypedBodyContent

final class TypeScope(inScope: Set[TypeId])
final class valueScope(inScope: Map[ID, TypeId])

abstract class TypedClass extends TypedBodyContent
final case class TypedActor()  extends TypedClass
final case class TypedTrait() extends TypedClass
final case class TypedObject() extends TypedClass

abstract class TopTypeException(message: String) extends Exception(message)
final class TraitNotFoundException(msg: String) extends TopTypeException(msg)
final class ClassExtendsNonTraitType(msg: String) extends TopTypeException(msg)
final class AbstractMethodNotImplemented(msg: String) extends TopTypeException(msg)
final class OverrideException(msg: String) extends TopTypeException(msg)
final class FormalArgsMismatch(msg: String) extends TopTypeException(msg)
final class ModuleNotFoundException(message: String) extends TopTypeException(message)
final class TypeNotFoundException(message: String) extends TopTypeException(message)
final class DuplicateTypeException(message: String) extends TopTypeException(message)