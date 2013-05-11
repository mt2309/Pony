package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.CompilationUnit.CompilationUnit
import com.github.mt2309.pony.AST._
import com.github.mt2309.pony.Common._
import com.github.mt2309.pony.AST.TypeBody

/**
 * User: mthorpe
 * Date: 30/04/2013
 * Time: 00:57
 */

final case class TypedModule(imports: Set[CompilationUnit], types: Map[TypeId, ModuleMember])
final case class PonyClass(name: TypeId, formalArgs: FormalArgs, is:List[TypeClass], typeBody: TypeBody)

abstract class TopTypeException(message: String) extends Exception(message)
final class TraitNotFoundException(msg: String) extends TopTypeException(msg)
final class ClassExtendsNonTraitType(msg: String) extends TopTypeException(msg)
final class AbstractMethodNotImplemented(msg: String) extends TopTypeException(msg)
final class OverrideException(msg: String) extends TopTypeException(msg)
final class FormalArgsMismatch(msg: String) extends TopTypeException(msg)
