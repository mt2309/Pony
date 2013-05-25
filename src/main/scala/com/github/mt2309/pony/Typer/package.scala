package com.github.mt2309.pony

import com.github.mt2309.pony.AST.{OfType, ModuleMember, Primitive}
import com.github.mt2309.pony.Common.ID

/**
 * User: mthorpe
 * Date: 19/05/2013
 * Time: 14:56
 */
package object Typer {

  type IFormalArgs = List[ITypeClass]
  type TFormalArgs = List[TTypeClass]

  type TArgs = List[TArg]

  val void: Primitive = new Primitive("Void")

  val bool: TPrimitive = new TPrimitive("Boolean")

  val boolOfType = new TOfType(Set(bool))

  val primitiveTypes: Set[ModuleMember] = Set(new Primitive("Int"), new Primitive("UInt"), new Primitive("Char"))

  // code duplication :(
  val tVoid = new TPrimitive("Void")
  val tPrimitiveTypes: Set[TModuleMember] = Set(new TPrimitive("Int"), new TPrimitive("UInt"), new TPrimitive("Char"))

  type VariableScope = Map[ID, TOfType]

}
