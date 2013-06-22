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

sealed abstract class IDMap
final class Var extends IDMap
final class Meth extends IDMap


final case class ClassData(currentClass: Option[ModuleMember] = None, isStatic: Boolean = false) {
  def inClass = currentClass.isDefined
  def name = currentClass.map(_.typeName).getOrElse("Outside class")
}

final case class Scope(typeScope: TypeScope = initialScope,
                       unTypedScope: UnTypedScope = Map.empty,
                       imports: CompilationUnits = new QualifiedCompilationUnits(Map.empty) -> new UnqualifiedCompilationUnits(Set.empty),
                       varScope: VariableScope = Map.empty, // Could include constants here: PI, E etc?
                       methScope: MethScope = Map.empty,
                       isList: Option[TIs] = None,
                       currentClass: ClassData = new ClassData,
                       checker: LowerTypeChecker = new LowerTypeChecker(Set.empty),
                       filename: Filename = "Primitive") extends NotNull
{
  def updateScope(id: ID, of: Option[TOfType])(implicit pos: Position): Scope = {
//    if (varScope.contains(id)) {
//      throw new VariableShadowingException(s"Variable $id, of type $of shadows variable with type ${this.varScope(id)}")(pos, this)
//    }
    val cp: Scope = this.copy(varScope = this.varScope + (id -> of))
    cp
  }

  def updateScope(id: ID, fun: TBodyContent): Scope = {
    this.copy(methScope = methScope + (id -> fun))
  }

  @tailrec
  def updateScope(varMap: Map[ID, Option[TOfType]])(implicit pos: Position): Scope = {
    if (varMap.isEmpty)
      this
    else
      this.updateScope(varMap.head._1, varMap.head._2).updateScope(varMap.tail)
  }

  def updateScope(typeId: TypeId)(implicit pos: Position): Scope = {
    if (typeScope.contains(typeId)) {
      throw new TypeShadowingException(s"Type $typeId, shadows type defined at ${this.typeScope(typeId)}")(pos, this)
    }
    else {
      this.copy(typeScope = typeScope + (typeId -> new EmptyType(typeId)(this).setPos(pos)))
    }
  }

  def mergeScope(that: Scope): Scope = {
    this.copy(varScope = varScope ++ that.varScope, methScope = methScope ++ that.methScope)
  }

  def setClass(optClazz: Option[ModuleMember]) = {
    this.copy(currentClass = currentClass.copy(currentClass = optClazz, isStatic = optClazz.exists(_.isStatic)))
  }

  def search(tClass: TypeClass): TModuleMember = {
    val p: Option[TModuleMember] = tPrimitiveTypes.find(_.name == tClass.name)
    val i: Option[TModuleMember] = if (p.isDefined) p else imports.searchType(tClass)
    val t: Option[TModuleMember] = if (i.isDefined) i else typeScope.get(tClass.name)


    if (t.isDefined) {
      t.get
    }
    else {
      val unTyped = unTypedScope.getOrElse(tClass.name, throw new TypeClassNotFoundException(s"${tClass.name} not found in ${tClass.fileName}")(tClass.pos, this))
      val scope = this.copy(filename = unTyped._1.fileName)
      checker.checkClass(unTyped._1, scope)
    }
  }

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

  def findMethod(id: ID, optOf: Option[TOfType])(implicit pos: Position): TBodyContent = optOf match {
    case None => {
      if (currentClass.inClass) {
        methScope.getOrElse(id, throw new MethodNotFoundException(id, currentClass.name)(pos, this))
      }
      else {
        throw new ThisTypeOutsideClass
      }
    }
    case Some(of) => {
      if (of.typeList.size == 0) throw new UntypedListException(id)(pos, this)
      val clazzList: Set[TModuleMember] = for (t <- of.typeList) yield t match {
        case p: TPartialType => p.typeclass.moduleMember
        case t: TTypeClass => t.moduleMember
        case l: TLambda => {
          throw new LambdaInMethCallException(s"Lambda found in method lookup for for id $id")(pos, this)
        }
        case t: TPrimitive => t
        case EmptyType(name) => {
          throw new EmptyTypeFound(s"Type Parameter $name found in method call, which have no methods defined on them")(pos, this)
        }
      }

      val methList: Set[TBodyContent] = for (c <- clazzList) yield c match {
        case p: TPrimitive => p.methods.getOrElse(id, throw new MethodNotFoundException(id, p.name)(pos, this))
        case EmptyType(name) => throw new EmptyTypeFound(s"Empty type $name found where it should not be")(pos, this)
        case p: TModuleMember => p.methods.getOrElse(id, throw new MethodNotFoundException(id, p.name)(pos, this))
      }

      val sizes = for (m <- methList) yield m -> methodExtract(m)
      val fst = sizes.head
      for (s <- sizes) {
        if (s._2._1 != fst._2._1 || s._2._2 != fst._2._2)
          throw new ArgumentMismatchException(s._1)(pos, this)
      }

      methList.head
    }
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

  def search(name: TypeId)(implicit pos: Position): TModuleMember = {
    val t: Option[TModuleMember] = typeScope.get(name)
    val i: Option[TModuleMember] = if (t.isDefined) t else imports.searchType(name)

    i.getOrElse {
      val unTyped = unTypedScope.getOrElse(name, throw new TypeClassNotFoundException(s"$name not found in $filename")(pos, this))
      val scope = this.copy(filename = unTyped._1.fileName)
      checker.checkClass(unTyped._1, scope)
    }
  }

  def findID(i: ID): Option[IDMap] = {
    if (varScope.contains(i)) Some(new Var)
    else if (methScope.contains(i)) Some(new Meth)
    else None
  }

  def searchID(i: ID)(implicit pos: Position): Option[TOfType] = {
    val variable = varScope.get(i)
    val meth = if (variable.isDefined) variable else methScope.get(i).map(_.ofType)

    meth.getOrElse(throw new VariableNotFoundException(s"$i not found")(pos, this))
  }
}
