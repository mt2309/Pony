package com.github.mt2309.pony

import com.github.mt2309.pony.AST.{ModuleMember, Primitive}
import com.github.mt2309.pony.Common.{ID, TypeId, ITypeScope}

/**
 * User: mthorpe
 * Date: 19/05/2013
 * Time: 14:56
 */
package object Typer {

  type IFormalArgs = List[ITypeClass]
  type TFormalArgs = List[TTypeClass]

  type TArgs = List[TArg]
  type TParams = List[TParam]

  val void: IPrimitive = new IPrimitive("Void")

  val bool: IPrimitive = new IPrimitive("Boolean")
  val pInt: IPrimitive = new IPrimitive("Int")
  val pDouble: IPrimitive = new IPrimitive("Double")
  val pUInt: IPrimitive = new IPrimitive("UInt")
  val pChar: IPrimitive = new IPrimitive("Char")

  val boolOfType = new TOfType(Set(bool.toTPrim))(new Scope)
  val intOfType = new TOfType(Set(pInt.toTPrim))(new Scope)
  val doubleOfType = new TOfType(Set(pDouble.toTPrim))(new Scope)
  val numericOfType = new TOfType(Set(pInt.toTPrim, pDouble.toTPrim, pUInt.toTPrim))(new Scope)

  val primitiveTypes: Set[ModuleMember] = Set(new Primitive("Int"), new Primitive("UInt"), new Primitive("Char"))
  val primMap: Map[TypeId, ModuleMember] = primitiveTypes.map(t => t.typeName -> t).toMap

  // code duplication :(
  val tVoid = new TPrimitive("Void")(new Scope)
  val tPrimitiveTypes: Set[TPrimitive] = Set(pChar.toTPrim, bool.toTPrim, pInt.toTPrim, pDouble.toTPrim, pUInt.toTPrim)
  val primScope: ITypeScope = tPrimitiveTypes.map(t => t.name -> t.toIPrim).toMap

  type VariableScope = Map[ID, TOfType]
}
