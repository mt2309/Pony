package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.Common._
import com.github.mt2309.pony.AST._

/**
 * User: mthorpe
 * Date: 25/05/2013
 * Time: 12:52
 */
final case class ITypedModule(imports: CompilationUnits, types: Map[TypeId, IModuleMember])(implicit val scope: Scope) extends NotNull

sealed abstract class IModuleMember(val name: TypeId) extends NotNull

final case class IPrimitive(typename: TypeId) extends IModuleMember(typename)
final case class IDeclare(override val name: TypeId, is: IIs, declareMap: DeclareMap) extends IModuleMember(name)
final case class IType(typename: TypeId, ofType: IOfType, is: IIs) extends IModuleMember(typename)

sealed abstract class IPonyClass(val na: TypeId, val formalArgs: IFormalArgs, val is:IIs, val typeBody: TypeBody) extends IModuleMember(na)
final case class IActor(n: TypeId, f: IFormalArgs, i:IIs, t: TypeBody)   extends IPonyClass(n,f,i,t)
final case class ITrait(n: TypeId, f: IFormalArgs, i:IIs, t: TypeBody)   extends IPonyClass(n,f,i,t)
final case class IObject(n: TypeId, f: IFormalArgs, i:IIs, t: TypeBody)  extends IPonyClass(n,f,i,t)

sealed trait ITypeElement extends NotNull

final case class IPartialType(name: ITypeClass)
  extends ITypeElement

final case class ITypeClass(iType: IModuleMember, mode: Mode = ReadOnly, formalArgs: FormalArgs = List.empty)
  extends ITypeElement

final case class ILambda(mode: Mode, args: List[Arg], result: Option[List[Arg]], throws: Boolean, block: Option[Block])
  extends ITypeElement

final case class IIs(list: List[ITypeClass]) extends NotNull
final case class IOfType(typeSet: Set[ITypeElement]) extends NotNull