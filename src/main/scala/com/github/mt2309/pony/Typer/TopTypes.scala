package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.AST._
import com.github.mt2309.pony.Common._

/**
 * User: mthorpe
 * Date: 30/04/2013
 * Time: 00:48
 */
final class TopTypes(val modules: Set[(Filename, Option[Module])]) extends TypeChecker {

  private val moduleScope: Map[TypeId, ModuleMember] = modules.map(_._2.map(_.classes)).flatten.flatten.toMap ++ primMap

  def topLevelTypes: Set[PreTypedModule] = modules.filter(_._2.isDefined).map(t => topLevelType(t._1, t._2.get))

  private def topLevelType(filename: Filename, module: Module): PreTypedModule = new TopTypeModule(filename, moduleScope, module).typeCheck.setPos(module.pos)
}
