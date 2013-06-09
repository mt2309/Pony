//package com.github.mt2309.pony.Typer
//
//import com.github.mt2309.pony.Common._
//import com.github.mt2309.pony.AST._
//import scala.util.parsing.input.Positional
//import annotation.tailrec
//
///**
// * User: mthorpe
// * Date: 25/05/2013
// * Time: 12:52
// */
//
//sealed trait ITyper extends NotNull with Positional
//
//final case class ITypedModule(imports: CompilationUnits, types: Map[TypeId, IModuleMember])(implicit val scope: Scope) extends ITyper
//
//sealed abstract class IModuleMember(val name: TypeId)(implicit val filename: Filename) extends ITyper {
//  def isSubType(that: IModuleMember): Boolean
//}
//
//final case class IPrimitive(typename: TypeId) extends IModuleMember(typename)(primitiveFilename) {
//  def toTPrim:TPrimitive = new TPrimitive(typename)(new Scope)
//
//  // No sub-typing on primitives.
//  override def isSubType(that: IModuleMember): Boolean = this == that
//}
//
//final case class EmptyType(typename: TypeId)(implicit override val filename: Filename) extends IModuleMember(typename) {
//  override def isSubType(that: IModuleMember): Boolean = this == that
//}
//
//final case class IDeclare(override val name: TypeId, is: IIs, declareMap: DeclareMap)(implicit override val filename: Filename) extends IModuleMember(name) {
//  override def isSubType(that: IModuleMember): Boolean = that match {
//    case i: IPrimitive => false
//    case i: IDeclare => false
//    case i: EmptyType => false
//    case i: IPonyClass => ???
//    case i: IType => ???
//  }
//}
//final case class IType(typename: TypeId, ofType: IOfType, is: IIs)(implicit override val filename: Filename) extends IModuleMember(typename) {
//  override def isSubType(that: IModuleMember): Boolean = ???
//}
//
//sealed abstract class IPonyClass(val na: TypeId, val formalArgs: IFormalArgs, val is:IIs, val typeBody: TypeBody)
//                                (implicit override val filename: Filename) extends IModuleMember(na) {
//  override def isSubType(that: IModuleMember): Boolean = ???
//}
//
//final case class IActor(typeName: TypeId, f: IFormalArgs, i:IIs, t: TypeBody)
//                       (implicit override val filename: Filename) extends IPonyClass(typeName,f,i,t)
//final case class ITrait(typeName: TypeId, f: IFormalArgs, i:IIs, t: TypeBody)
//                       (implicit override val filename: Filename) extends IPonyClass(typeName,f,i,t)
//final case class IObject(typeName: TypeId, f: IFormalArgs, i:IIs, t: TypeBody, isStatic: Boolean)
//                        (implicit override val filename: Filename) extends IPonyClass(typeName,f,i,t)
//
//sealed trait ITypeElement extends ITyper
//final case class IPartialType(name: ITypeClass) extends ITypeElement
//final case class ITypeClass(iType: IModuleMember, mode: Mode = ReadOnly, formalArgs: FormalArgs = List.empty) extends ITypeElement
//final case class ILambda(mode: Mode, args: Args, result: Params, throws: Boolean, block: Option[Block])
//  extends ITypeElement
//
//final case class IIs(list: List[ITypeClass]) extends ITyper
//final case class IOfType(typeSet: Set[ITypeElement]) extends ITyper
