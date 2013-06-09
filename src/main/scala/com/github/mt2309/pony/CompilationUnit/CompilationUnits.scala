package com.github.mt2309.pony.CompilationUnit

import com.github.mt2309.pony.Common._
import com.github.mt2309.pony.Typer.{TModuleMember, ModuleNotFoundException}
import scala.util.parsing.input.Position

/**
 * User: mthorpe
 * Date: 13/05/2013
 * Time: 16:00
 */

class UnqualifiedCompilationUnits(val units: Set[CompilationUnit]) extends AnyVal {
  def lookUpType(name: TypeId): Option[TModuleMember] = units.find(p => p.searchType(name).isDefined).map(_.searchType(name)).flatten
}

class QualifiedCompilationUnits(val units: Map[TypeId, CompilationUnit]) extends AnyVal {
  def lookUpType(name: TypeId, qualifier: TypeId)(implicit pos: Position): Option[TModuleMember] = {
    units.getOrElse(qualifier, throw new ModuleNotFoundException(s"Module typeClass $qualifier not found")).searchType(name)
  }
}
