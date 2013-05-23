package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.Common._
import com.github.mt2309.pony.AST._
import com.github.mt2309.pony.CompilationUnit.{QualifiedCompilationUnits, UnqualifiedCompilationUnits}
import com.github.mt2309.pony.Loader.Loader
import com.github.mt2309.pony.AST.Actor
import com.github.mt2309.pony.AST.Module
import com.github.mt2309.pony.AST.Trait
import com.github.mt2309.pony.AST.Object

/**
 * User: mthorpe
 * Date: 21/05/2013
 * Time: 15:57
 */
class TopTypeModule(val filename: Filename, moduleScope: TypeScope, module: Module) {

  val imports: UnqualifiedCompilationUnits = {
    val unqualifiedIm = for (i <- module.imports.filter(_.toType.isEmpty)) yield Loader.load(filename, i.importName)
    new UnqualifiedCompilationUnits(unqualifiedIm)
  }

  val typedImports: QualifiedCompilationUnits = {
    val qualified = (for (i <- module.imports.filter(_.toType.isDefined)) yield i.toType.get -> Loader.load(filename, i.importName)).toMap
    new QualifiedCompilationUnits(qualified)
  }

  implicit val scope: TypeScope = moduleScope ++ imports.units.map(_.typeScope).flatten

  // Given that we've loaded all of the imports and compiled them, we can now check whether traits are correct
  for (c <- module.classes) {
    c._2 match {
      case a: Actor => checkTraitsFinal(a)
      case t: Trait => checkTraitsTrait(t)
      case o: Object => checkTraitsFinal(o)
      case d: Declare => checkTraitsDeclare(d)
      case p: Primitive => throw new RuntimeException("This should never happen")
      case t: Type => ???
    }
  }

  def typeCheck: PreTypedModule = new PreTypedModule(typedImports -> imports, module.classes)

  def checkTraitsFinal(pc: PonyParserClass) {
    val traitList = pc.is.list.map(typeClass => typeClass -> findTrait(typeClass.name))

    // for every trait we import, check that that all the abstract methods are implemented
    // And check that they're the same kind
    for (t <- traitList; m <- t._2.typeBody.body; if m._2.isAbstract) {
      val method: BodyContent = pc.typeBody.body.getOrElse(m._1,
        throw new AbstractMethodNotImplemented(s"Method ${m._1} declared abstract in trait ${t._2.name} and not implemented in class ${pc.name}"))

      if (method.getClass != m.getClass)
        throw new OverrideException(s"Implemented method is wrong kind - expected ${m._2.getClass.getSimpleName}, got ${method.getClass.getSimpleName}")
    }

    // Check formal arguments match
    for (t <- traitList) {
      if (!TopTypes.compareFormalArgs(t._1.formalArgs, t._2.formalArgs))
        throw new FormalArgsMismatch(s"Formal args in mixed-in trait ${t._1.name} do not match defined formal args in trait ${t._2.name} from file $filename")

      t._1.formalArgs.map(_.map(findType(_)))
    }


  }

  def checkTraitsTrait(ponyTrait: Trait) {
    val traitList = ponyTrait.is.list.map(typeclass => typeclass -> findTrait(typeclass.name))

    for (t <- traitList) {
      if (!TopTypes.compareFormalArgs(t._1.formalArgs, t._2.formalArgs))
        throw new FormalArgsMismatch(s"Formal args in mixed-in trait ${t._1.name} do not match defined formal args in trait ${t._2.name} from file $filename")

      t._1.formalArgs.map(_.map(findType(_)))
    }
  }

  def checkTraitsDeclare(declare: Declare) {
    val decList = declare.is.list.map(typeClass => typeClass -> checkTypeClass(typeClass))

    for (t <- decList) {
      if (!TopTypes.compareFormalArgs(t._1.formalArgs, t._2.formalArgs))
        throw new FormalArgsMismatch(s"Formal args in mixed-in trait ${t._1.name} do not match defined formal args in trait ${t._2.name} from file $filename")

      t._1.formalArgs.map(_.map(findType(_)))
    }
  }

  def findType(name: TypeId): ModuleMember = {
    val prim = primitiveTypes.find(_.name == name)

    val sc = if (prim.isDefined) prim else scope.get(name)

    if (sc.isDefined)
      sc.get
    else
      throw new TypeNotFoundException(s"Could not find type $name from module $filename")
  }

  def findTrait(typename: TypeId): Trait = {

    val t = findType(typename)

    t match {
      case t: Trait => t
      case _ => throw new ClassExtendsNonTraitType(s"Class extends nontrait type $typename in module $filename")
    }
  }

  def checkTypeClass(typeclass: TypeClass): TypeClass = {
    typeclass.module match {
      case Some(x) => {
        typedImports.lookUpType(typeclass.name, x, filename).getOrElse(throw new TypeNotFoundException(s"Type ${typeclass.name}::$x not found from $filename"))
      }
      case None => findType(typeclass.name)
    }

    typeclass
  }
}
