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

  // this.type - TODO: incomplete
  val thisOfType = new TOfType(Set())(pScope)

  val primitiveTypes: Set[AST.ModuleMember] = Set(new AST.Primitive("Int"), new AST.Primitive("UInt"), new AST.Primitive("Char"))
  val primMap: Map[TypeId, AST.ModuleMember] = primitiveTypes.map(t => t.typeName -> t).toMap

  val tVoid = new TPrimitive("Void")(new Scope)
  val tPrimitiveTypes: Set[TPrimitive] = Set(pChar.toTPrim, bool.toTPrim, pInt.toTPrim, pDouble.toTPrim, pUInt.toTPrim)
  val primTOfType = new TOfType(tPrimitiveTypes.asInstanceOf[Set[TTypeElement]])(pScope)
  val primScope: ITypeScope = tPrimitiveTypes.map(t => t.name -> t.toIPrim).toMap

  type VariableScope = Map[ID, TOfType]

}

object ImplicitTraits {

  implicit val scope = pScope
  implicit val filename = "Implicit trait"

  val intOf = new AST.ConcreteOfType(Set(new AST.TypeClass("Int")))

  val Actor: ITrait = new ITrait("Actor", List.empty, new IIs(List.empty), new AST.TypeBody(Map.empty))

  val Hashable: ITrait = new ITrait("Hashable", List.empty, new IIs(List.empty),
    new AST.TypeBody(Map("hash" -> new AST.Function(
      new AST.MethodContent(AST.ReadOnly, "hash", new AST.CombinedArgs(List.empty, List.empty)), List(new AST.Param("hash", intOf)), false, Some(new AST.Block(List.empty, None, None))))))

  val Partial: ITrait = new ITrait("Partial", List.empty, new IIs(List.empty),
    new AST.TypeBody(Map("mirror" -> new AST.Function(
      new AST.MethodContent(AST.ReadOnly, "mirror", new AST.CombinedArgs(List.empty, List.empty)), List(new AST.Param("mirror", new AST.ThisOfType)), false, Some(new AST.Block(List.empty, None, None))))))

  val allTraits: List[ITypeClass] = List(Hashable, Partial).map(new ITypeClass(_))
  val actorTraits = new ITypeClass(Actor) :: allTraits
}
