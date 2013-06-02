package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.Common._
import com.github.mt2309.pony.CompilationUnit.{UnqualifiedCompilationUnits, QualifiedCompilationUnits}
import com.github.mt2309.pony.AST._
import scala.util.parsing.input.Position

/**
 * User: mthorpe
 * Date: 28/05/2013
 * Time: 21:36
 */

final case class ClassData(currentClass: Option[IModuleMember] = None, isStatic: Boolean = false)

final case class Scope(typeScope: ITypeScope = primScope,
                       imports: CompilationUnits = (new QualifiedCompilationUnits(Map.empty) -> new UnqualifiedCompilationUnits(Set.empty)),
                       varScope: VariableScope = Map.empty, // Could include constants here: PI, E etc?
                       currentClass: ClassData = new ClassData,
                       filename: Filename = "Primitive") extends NotNull
{
  def updateScope(id: ID, of: TOfType)(implicit pos: Position): Scope = {
    if (varScope.contains(id)) {
      throw new VariableShadowingException(s"Variable $id, of type $of shadows variable with type ${this.varScope.get(id)}")(pos, this)
    }
    val cp: Scope = this.copy(varScope = (this.varScope + (id -> of)))
    cp
  }

  def updateScope(typeId: TypeId)(implicit pos: Position): Scope = {
    if (typeScope.contains(typeId)) {
      throw new TypeShadowingException(s"Type $typeId, shadows type defined at ${this.typeScope(typeId)}")(pos, this)
    }
    else
      this.copy(typeScope = typeScope + (typeId -> new EmptyType(typeId)(filename).setPos(pos)))
  }

  def mergeScope(that: Scope): Scope = this.copy(varScope = varScope ++ that.varScope)

  def setClass(optClazz: Option[IModuleMember]) = this.copy(currentClass = currentClass.copy(currentClass = optClazz))


  def search(t: TypeClass): IModuleMember = {
    val p: Option[IModuleMember] = primScope.get(t.name)
    val i: Option[IModuleMember] = if (p.isDefined) p else imports.searchType(t)

    i.getOrElse(typeScope.getOrElse(t.name, throw new TypeClassNotFoundException(s"$t not found in ${t.fileName}")(t.pos, this)))
  }

  def search(t: TTypeClass): IModuleMember = t.moduleMember

  def search(m: ModuleMember): IModuleMember = search(m.typeName)(m.pos)

  def checkIsList(i: IIs)(implicit pos: Position): Boolean = {
    for (mem <- i.list) {
      search(mem.iType.name)
    }

    true
  }

  def findMethod(id: ID, of: TOfType)(implicit pos: Position): BodyContent = {
    if (of.typeList.size == 0) throw new UntypedListException(id)(pos, this)
    val clazzList: Set[IModuleMember] = for (t <- of.typeList) yield t match {
      case p: TPartialType => search(p.name)
      case t: TTypeClass => search(t)
      case l: TLambda => throw new LambdaInMethCallException(s"Lambda found in method lookup for for id $id")(pos, this)
      case TPrimitive(name) => throw new PrimitiveFound(s"Primitive $name found in method call, which have no methods defined on them")(pos, this)
    }

    val methList: Set[BodyContent] = for (c <- clazzList) yield c match {
      case IPrimitive(name) => throw new PrimitiveFound(s"Primitive $name found in method call, which have no methods defined on them")(pos, this)
      case EmptyType(name) => throw new EmptyTypeFound(s"Empty type $name found where it should not be")(pos, this)
      case IActor(name, _, is, t) => {
        val body = t.body.get(id)
        body.getOrElse(findMethod(id, is)) //throw new MethodNotFoundException(id, name)(pos, this))
      }
      case IObject(_, _, is, t, _) => {
        val body = t.body.get(id)
        body.getOrElse(findMethod(id, is))
      }
      case ITrait(_, _, is, t) => {
        val body = t.body.get(id)
        body.getOrElse(findMethod(id, is))
      }
      case IDeclare(_, is, map) => {
        val declareID: ID = map.map.find(_.to == id).map(_.from).getOrElse(id)
        findMethod(declareID, is)
      }
      case IType(_, o, is) => {
        val oBody = findMethod(id, o)
        val body = findMethod(id, is)
        ???
      }
    }

    val sizes = for (m <- methList) yield (m -> methodExtract(m))
    val fst = sizes.head
    for (s <- sizes) {
      if (s._2._1 != fst._2._1 || s._2._2 != fst._2._2)
        throw new ArgumentMismatchException(s._1)(pos, this)
    }

    methList.head
  }

  // Length of inputs args, length of output args
  def methodExtract(b: BodyContent): (Int, Int) = b match {
    case Field(name, ofType, expr) => (0, 1)
    case Delegate(name, ofType) => (0, 1)
    case Constructor(contents, throws, block) => (contents.combinedArgs.args.length, 1)
    case Ambient(contents, throws, block) => (contents.combinedArgs.args.length, 0)
    case Function(contents, results, throws, block) => (contents.combinedArgs.args.length, results.length)
    case Message(contents, block) => (contents.combinedArgs.args.length, 0)
  }

  def findMethod(id: ID, is: IOfType): BodyContent = ???

  def findMethod(id: ID, is: IIs): BodyContent = {
    ???
  }

  def search(t: TypeId)(implicit pos: Position): IModuleMember = typeScope.getOrElse(t, throw new TypeNotFoundException(t))

  def searchID(i: ID)(implicit pos: Position): TOfType = varScope.getOrElse(i, throw new VariableNotFoundException(s"$i not found")(pos, this))
}
