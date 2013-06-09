package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.Common._
import com.github.mt2309.pony.CompilationUnit.{UnqualifiedCompilationUnits, QualifiedCompilationUnits}
import com.github.mt2309.pony.AST._
import scala.util.parsing.input.Position
import annotation.tailrec

/**
 * User: mthorpe
 * Date: 28/05/2013
 * Time: 21:36
 */

final case class ClassData(currentClass: Option[ModuleMember] = None, isStatic: Boolean = false)

final case class Scope(typeScope: ITypeScope = primScope,
                       imports: CompilationUnits = new QualifiedCompilationUnits(Map.empty) -> new UnqualifiedCompilationUnits(Set.empty),
                       varScope: VariableScope = Map.empty, // Could include constants here: PI, E etc?
                       currentClass: ClassData = new ClassData,
                       filename: Filename = "Primitive") extends NotNull
{
  def updateScope(id: ID, of: TOfType)(implicit pos: Position): Scope = {
    if (varScope.contains(id)) {
      throw new VariableShadowingException(s"Variable $id, of type $of shadows variable with type ${this.varScope.get(id)}")(pos, this)
    }
    val cp: Scope = this.copy(varScope = this.varScope + (id -> of))
    cp
  }

  @tailrec
  def updateScope(varMap: Map[ID, TOfType], lower: LowerTypeChecker)(implicit pos: Position): Scope = {
    if (varMap.isEmpty)
      this
    else
      this.updateScope(varMap.head._1, varMap.head._2).updateScope(varMap.tail, lower)
  }

  def updateScope(typeId: TypeId)(implicit pos: Position): Scope = {
    if (typeScope.contains(typeId)) {
      throw new TypeShadowingException(s"Type $typeId, shadows type defined at ${this.typeScope(typeId)}")(pos, this)
    }
    else {
      this.copy(typeScope = typeScope + (typeId -> new EmptyType(typeId)(this).setPos(pos)))
    }
  }

  def mergeScope(that: Scope): Scope = this.copy(varScope = varScope ++ that.varScope)

  def setClass(optClazz: Option[ModuleMember]) = this.copy(currentClass = currentClass.copy(currentClass = optClazz))


  def search(t: TypeClass): TModuleMember = {
    val p: Option[TModuleMember] = tPrimitiveTypes.find(_.typename == t.name)
    val i: Option[TModuleMember] = if (p.isDefined) p else imports.searchType(t)

    i.getOrElse(typeScope.getOrElse(t.name, throw new TypeClassNotFoundException(s"$t not found in ${t.fileName}")(t.pos, this)))
  }

  def search(t: TTypeClass): TModuleMember = t.moduleMember

  def search(m: ModuleMember): TModuleMember = search(m.typeName)(m.pos)

  def checkIsList(i: Is)(implicit pos: Position): Boolean = {
    for (mem <- i.list) {
      search(mem.name)
    }

    true
  }

  def findMethod(id: ID, is: TIs)(implicit pos: Position): TBodyContent = {
    is.methods.getOrElse(id, throw new MethodNotFoundException(id, this.filename)(pos, this))
  }

  def findMethod(id: ID, of: TOfType)(implicit pos: Position): TBodyContent = {
    if (of.typeList.size == 0) throw new UntypedListException(id)(pos, this)
    val clazzList: Set[TModuleMember] = for (t <- of.typeList) yield t match {
      case p: TPartialType => search(p.typeclass)
      case t: TTypeClass => search(t)
      case l: TLambda => throw new LambdaInMethCallException(s"Lambda found in method lookup for for id $id")(pos, this)
      case TPrimitive(name) => throw new PrimitiveFound(s"Primitive $name found in method call, which have no methods defined on them")(pos, this)
    }

    val methList: Set[TBodyContent] = for (c <- clazzList) yield c match {
      case TPrimitive(name) => throw new PrimitiveFound(s"Primitive $name found in method call, which have no methods defined on them")(pos, this)
      case EmptyType(name) => throw new EmptyTypeFound(s"Empty type $name found where it should not be")(pos, this)
      case p: PonyClass => p.methods.getOrElse(id, throw new MethodNotFoundException(id, p.name)(pos, this))
      case t: TDeclare => t.methods.getOrElse(id, throw new MethodNotFoundException(id, t.name)(pos, this))
      case t: TType => t.methods.getOrElse(id, throw new MethodNotFoundException(id, t.name)(pos, this))
    }

    val sizes = for (m <- methList) yield m -> methodExtract(m)
    val fst = sizes.head
    for (s <- sizes) {
      if (s._2._1 != fst._2._1 || s._2._2 != fst._2._2)
        throw new ArgumentMismatchException(s._1)(pos, this)
    }

    methList.head
  }

  // Length of inputs args, length of output args
  def methodExtract(b: TBodyContent): (Int, Int) = b match {
    case TField(name, ofType, expr) => (0, 1)
    case TDelegate(name, ofType) => (0, 1)
    case TConstructor(contents, throws, block) => (contents.combinedArgs.args.length, 1)
    case TAmbient(contents, throws, block) => (contents.combinedArgs.args.length, 0)
    case TFunction(contents, results, throws, block) => (contents.combinedArgs.args.length, results.length)
    case TMessage(contents, block) => (contents.combinedArgs.args.length, 0)
  }

  def search(t: TypeId)(implicit pos: Position): TModuleMember = typeScope.getOrElse(t, throw new TypeNotFoundException(t))

  def searchID(i: ID)(implicit pos: Position): TOfType = {
    varScope.getOrElse(i, throw new VariableNotFoundException(s"$i not found")(pos, this))
  }
}
