package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.Common._
import com.github.mt2309.pony.AST._

/**
 * User: mthorpe
 * Date: 11/05/2013
 * Time: 16:30
 */
final class LowerTypeChecker(val topTypes: Set[PreTypedModule]) {

  def typeCheck: Set[TypedModule] = {topTypes.map(checkModule(_))}

  def checkModule(module: PreTypedModule): TypedModule = {
    implicit val typeScope: TypeScope = module.typeScope
    new TypedModule(module.imports, module.classes.map(c => c._1 -> checkClass(c._2)))
  }

  def checkClass(moduleMember: ModuleMember)(implicit scope: TypeScope): TModuleMember = ???

  def checkDeclare(declare: Declare)(implicit scope: TypeScope): TDeclare = ???

  def checkPonyClass(pc: PonyParserClass)(implicit scope: TypeScope): PonyClass = ???


}