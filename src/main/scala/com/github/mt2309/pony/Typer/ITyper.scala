package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.Common._
import com.github.mt2309.pony.AST._
import scala.util.parsing.input.Positional
import annotation.tailrec

/**
 * User: mthorpe
 * Date: 25/05/2013
 * Time: 12:52
 */

sealed trait ITyper extends NotNull with Positional

final case class ITypedModule(imports: CompilationUnits, types: Map[TypeId, IModuleMember])(implicit val scope: Scope) extends ITyper

sealed abstract class IModuleMember(val name: TypeId)(implicit val filename: Filename) extends ITyper {
  def isSubType(that: IModuleMember): Boolean
  def getVariables: Map[ID, OfType]
}

final case class IPrimitive(typename: TypeId) extends IModuleMember(typename)(primitiveFilename) {
  def toTPrim:TPrimitive = new TPrimitive(typename)(new Scope)

  // No sub-typing on primitives.
  override def isSubType(that: IModuleMember): Boolean = this == that

  override def getVariables: Map[ID, OfType] = Map.empty
}

final case class EmptyType(typename: TypeId)(implicit override val filename: Filename) extends IModuleMember(typename) {
  override def isSubType(that: IModuleMember): Boolean = this == that
  override def getVariables: Map[ID, OfType] = Map.empty
}

final case class IDeclare(override val name: TypeId, is: IIs, declareMap: DeclareMap)(implicit override val filename: Filename) extends IModuleMember(name) {
  override def isSubType(that: IModuleMember): Boolean = that match {
    case i: IPrimitive => false
    case i: IDeclare => false
    case i: EmptyType => false
    case i: IPonyClass => ???
    case i: IType => ???
  }

  override def getVariables: Map[ID, OfType] = is.getVariables
}
final case class IType(typename: TypeId, ofType: IOfType, is: IIs)(implicit override val filename: Filename) extends IModuleMember(typename) {
  override def isSubType(that: IModuleMember): Boolean = ???

  def getVariables: Map[ID, OfType] = is.getVariables
}

sealed abstract class IPonyClass(val na: TypeId, val formalArgs: IFormalArgs, val is:IIs, val typeBody: TypeBody)
                                (implicit override val filename: Filename) extends IModuleMember(na) {
  override def isSubType(that: IModuleMember): Boolean = ???

  override def getVariables: Map[ID, OfType] = {
    val fields: Map[ID, OfType] = typeBody.body.filter(_._2.isInstanceOf[Field]).map(t => t._1 -> t._2.asInstanceOf[Field].ofType)

    ITyperHelper.reduceVariables(List(fields,is.getVariables))
  }

}

final case class IActor(n: TypeId, f: IFormalArgs, i:IIs, t: TypeBody)
                       (implicit override val filename: Filename) extends IPonyClass(n,f,i,t)
final case class ITrait(n: TypeId, f: IFormalArgs, i:IIs, t: TypeBody)
                       (implicit override val filename: Filename) extends IPonyClass(n,f,i,t)
final case class IObject(n: TypeId, f: IFormalArgs, i:IIs, t: TypeBody, isStatic: Boolean)
                        (implicit override val filename: Filename) extends IPonyClass(n,f,i,t)

sealed trait ITypeElement extends ITyper

final case class IPartialType(name: ITypeClass)
  extends ITypeElement

final case class ITypeClass(iType: IModuleMember, mode: Mode = ReadOnly, formalArgs: FormalArgs = List.empty) extends ITypeElement {
  def getVariables: Map[ID, OfType] = iType.getVariables
}

final case class ILambda(mode: Mode, args: Args, result: Params, throws: Boolean, block: Option[Block])
  extends ITypeElement

final case class IIs(list: List[ITypeClass]) extends ITyper {
  def getVariables: Map[ID, OfType] = ITyperHelper.reduceVariables(for (iType <- list) yield iType.getVariables)
}
final case class IOfType(typeSet: Set[ITypeElement]) extends ITyper

object ITyperHelper {

  def reduceVariables(list: List[Map[ID, OfType]]): Map[ID, OfType] = reduceVariablesHelper(list, Map.empty)

  @tailrec
  private def reduceVariablesHelper(list: List[Map[ID, OfType]], map: Map[ID, OfType]): Map[ID, OfType] = list match {
    case x :: xs => {
      var m = map
      for (varD <- x) {
        if (!m.contains(varD._1)) m += varD
      }
      reduceVariablesHelper(xs, m)
    }
    case Nil => map
  }
}
