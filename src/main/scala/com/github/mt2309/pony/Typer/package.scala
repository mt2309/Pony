package com.github.mt2309.pony

import com.github.mt2309.pony.Common.{ID, TypeId, CompilationUnits}

import language.implicitConversions

/**
 * User: mthorpe
 * Date: 19/05/2013
 * Time: 14:56
 */
package object Typer {


  type UnTypedScope = Map[TypeId, (AST.ModuleMember, CompilationUnits)]
  type TypeScope = Map[TypeId, TModuleMember]

  //  type IFormalArgs = List[ITypeClass]
  type TFormalArgs = List[TTypeClass]

  type TArgs = List[TArg]
  type TParams = List[TParam]

  val pScope = new Scope

  val void: TPrimitive = new TPrimitive("Void")(pScope)

  val bool: TPrimitive = new TPrimitive("Boolean")(pScope)
  val pInt: TPrimitive = new TPrimitive("Int")(pScope)
  val pDouble: TPrimitive = new TPrimitive("Double")(pScope)
  val pUInt: TPrimitive = new TPrimitive("UInt")(pScope)
  val pChar: TPrimitive = new TPrimitive("Char")(pScope)
  val pString: TPrimitive = new TPrimitive("String")(pScope)

  val boolOfType = new TOfType(Set(bool))(pScope)
  val intOfType = new TOfType(Set(pInt))(pScope)
  val stringOfType = new TOfType(Set(pString))(pScope)
  val doubleOfType = new TOfType(Set(pDouble))(pScope)
  val numericOfType = new TOfType(Set(pInt, pDouble, pUInt))(pScope)

  // this.type - TODO: incomplete
  val thisOfType = new TOfType(Set())(pScope)

  val primitiveTypes: Set[AST.ModuleMember] = Set(new AST.Primitive("Int"), new AST.Primitive("UInt"), new AST.Primitive("Char"))
  val primMap: Map[TypeId, AST.ModuleMember] = primitiveTypes.map(t => t.typeName -> t).toMap

  val tVoid = new TPrimitive("Void")(new Scope)
  val tPrimitiveTypes: Set[TPrimitive] = Set(pChar, bool, pInt, pDouble, pUInt)
  val primTOfType = new TOfType(tPrimitiveTypes.asInstanceOf[Set[TTypeElement]])(pScope)
  val primScope: TypeScope = tPrimitiveTypes.map(t => t.name -> t).toMap

  type VariableScope = Map[ID, Option[TOfType]]
  type MethScope = Map[ID, TBodyContent]
}
