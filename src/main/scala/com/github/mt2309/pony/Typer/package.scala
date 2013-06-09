package com.github.mt2309.pony

import com.github.mt2309.pony.Common.{ID, TypeId, ITypeScope}

import language.implicitConversions
import Typer._

/**
 * User: mthorpe
 * Date: 19/05/2013
 * Time: 14:56
 */
package object Typer {

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
  val primScope: ITypeScope = tPrimitiveTypes.map(t => t.name -> t).toMap

  type VariableScope = Map[ID, TOfType]

}

object ImplicitTraits {

  implicit val scope = pScope
  implicit val filename = "Implicit trait"

  val Actor: TTrait = new TTrait("Actor", List.empty, new TIs(List.empty), new TTypeBody(Map.empty))

  val Hashable: TTrait = new TTrait("Hashable", List.empty, new TIs(List.empty),
    new TTypeBody(Map("hash" -> new TFunction(
      new TMethodContent(new TReadOnly, "hash", new TCombinedArgs(List.empty, List.empty)), List(new TParam("hash", intOfType)), false, Some(new TBlock(List.empty, None, None))))))

  val Partial: TTrait = new TTrait("Partial", List.empty, new TIs(List.empty),
    new TTypeBody(Map("mirror" -> new TFunction(
      new TMethodContent(new TReadOnly, "mirror", new TCombinedArgs(List.empty, List.empty)), List(new TParam("mirror", thisOfType)), false, Some(new TBlock(List.empty, None, None))))))

  val allTraits: List[TTypeClass] = List(Hashable, Partial).map(new TTypeClass(_))
  val actorTraits = new TTypeClass(Actor) :: allTraits
}
