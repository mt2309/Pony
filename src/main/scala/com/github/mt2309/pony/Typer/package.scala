package com.github.mt2309.pony

import com.github.mt2309.pony.AST.{ModuleMember, Primitive}
import com.github.mt2309.pony.Common.{ID, TypeId, ITypeScope}

import language.implicitConversions

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

  val pScope = new Scope

  val void: IPrimitive = new IPrimitive("Void")

  val bool: IPrimitive = new IPrimitive("Boolean")
  val pInt: IPrimitive = new IPrimitive("Int")
  val pDouble: IPrimitive = new IPrimitive("Double")
  val pUInt: IPrimitive = new IPrimitive("UInt")
  val pChar: IPrimitive = new IPrimitive("Char")

  val boolOfType = new TOfType(Set(bool.toTPrim))(pScope)
  val intOfType = new TOfType(Set(pInt.toTPrim))(pScope)
  val doubleOfType = new TOfType(Set(pDouble.toTPrim))(pScope)
  val numericOfType = new TOfType(Set(pInt.toTPrim, pDouble.toTPrim, pUInt.toTPrim))(pScope)

  val primitiveTypes: Set[ModuleMember] = Set(new Primitive("Int"), new Primitive("UInt"), new Primitive("Char"))
  val primMap: Map[TypeId, ModuleMember] = primitiveTypes.map(t => t.typeName -> t).toMap

  // code duplication :(
  val tVoid = new TPrimitive("Void")(new Scope)
  val tPrimitiveTypes: Set[TPrimitive] = Set(pChar.toTPrim, bool.toTPrim, pInt.toTPrim, pDouble.toTPrim, pUInt.toTPrim)
  val primTOfType = new TOfType(tPrimitiveTypes.asInstanceOf[Set[TTypeElement]])(pScope)
  val primScope: ITypeScope = tPrimitiveTypes.map(t => t.name -> t.toIPrim).toMap

  type VariableScope = Map[ID, TOfType]
}
