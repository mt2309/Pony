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

  val bool: TPrimitive = new TPrimitive("Boolean")(new Scope)
  val pInt: TPrimitive = new TPrimitive("Int")(new Scope)
  val pUInt: TPrimitive = new TPrimitive("UInt")(new Scope)
  val pChar: TPrimitive = new TPrimitive("Char")(new Scope)

  val boolOfType = new TOfType(Set(bool))(new Scope)

  val primitiveTypes: Set[ModuleMember] = Set(new Primitive("Int"), new Primitive("UInt"), new Primitive("Char"))

  // code duplication :(
  val tVoid = new TPrimitive("Void")(new Scope)
  val tPrimitiveTypes: Set[TModuleMember] = Set(bool, pInt, pUInt, pChar)

  type VariableScope = Map[ID, TOfType]

}
