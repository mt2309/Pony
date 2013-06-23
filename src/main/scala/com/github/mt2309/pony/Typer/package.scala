package com.github.mt2309.pony

import com.github.mt2309.pony.Common.{ID, TypeId, CompilationUnits}

import language.implicitConversions

/**
 * User: mthorpe
 * Date: 19/05/2013
 * Time: 14:56
 */
package object Typer {

  var implicitSender: Boolean = true


  type UnTypedScope = Map[TypeId, (AST.ModuleMember, CompilationUnits)]
  type TypeScope = Map[TypeId, TModuleMember]

  type TFormalArgs = List[TTypeElement]

  type TArgs = List[TArg]
  type TParams = List[TParam]

  val pScope = new Scope

  val void: TPrimitive = new TPrimitive("Void", "void", "(void)0")(pScope)
  val bool: TPrimitive = new TPrimitive("Bool", "bool", "false")(pScope)
  val pInt: TPrimitive = new TPrimitive("Int", "int", "0")(pScope)
  val pDouble: TPrimitive = new TPrimitive("Double", "double", "0.0")(pScope)
  val pUInt: TPrimitive = new TPrimitive("UInt", "unsigned int", "0")(pScope)

  val pChar: TPrimitive = new TPrimitive("Char", "char", "0")(pScope)
  val pString: TPrimitive = new TPrimitive("String", "char *", "\"\"")(pScope)

  val boolOfType = new TOfType(Set(bool))(pScope)
  val intOfType = new TOfType(Set(pInt))(pScope)
  val voidOfType = new TOfType(Set(void))(pScope)
  val stringOfType = new TOfType(Set(pString))(pScope)
  val doubleOfType = new TOfType(Set(pDouble))(pScope)
  val numericOfType = new TOfType(Set(pInt, pDouble, pUInt))(pScope)

  val primitiveTypes: Set[AST.ModuleMember] = Set(new AST.Primitive("Int"), new AST.Primitive("UInt"), new AST.Primitive("Char"))
  val primMap: Map[TypeId, AST.ModuleMember] = primitiveTypes.map(t => t.typeName -> t).toMap

  val tPrimitiveTypes: Set[TPrimitive] = Set(pChar, bool, pInt, pDouble, pUInt)
  val primTOfType = new TOfType(tPrimitiveTypes.asInstanceOf[Set[TTypeElement]])(pScope)
  val primScope: TypeScope = tPrimitiveTypes.map(t => t.name -> t).toMap

  val initialScope: TypeScope = primScope ++ ImplicitTraits.implicitTraits

  type VariableScope = Map[ID, Var]
  type MethScope = Map[ID, TBodyContent]
}
