package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.AST._
import com.github.mt2309.pony.Common._

import com.github.mt2309.pony.CompilationUnit.CompilationUnit
import com.github.mt2309.pony.Loader.Loader

/**
 * User: mthorpe
 * Date: 30/04/2013
 * Time: 00:48
 */
final class TopTypes(val modules:Set[(Filename, Option[Module])]) extends TypeChecker {

  def topLevelTypes: Set[TypedModule] = modules.filter(_._2.isDefined).map(t => topLevelType(t._1, t._2.get))

  def topLevelType(filename: String, module: Module): TypedModule = {
    val imports = for (i <- module.imports) yield Loader.load(filename, i.importName)

    // Given that we've loaded all of the imports and compiled them, we can now check whether traits are correct

    for (c <- module.classes) {
      c._2 match {
        case a:Actor => checkTraitsFinal(a)
        case t:Trait => checkTraitsTrait(t)
        case o:Object => checkTraitsFinal(o)
        case d:Declare => checkTraitsDeclare(d)
      }
    }

    def checkTraitsFinal(pc: PonyParserClass) {
      pc.is match {
        case Some(is) => {
          val traitList = is.list.map(typeClass => typeClass -> findTrait(typeClass.name))

          // for every trait we import, check that that all the abstract methods are implemented
          // And check that they're the same kind
          for (t <- traitList; m <- t._2.typeBody.body; if m._2.isAbstract) {
            val method: BodyContent = pc.typeBody.body.getOrElse(m._1,throw new AbstractMethodNotImplemented(s"Method ${m._1} declared abstract in trait ${t._2.name} and not implemented in class ${pc.name}"))

            if (method.getClass != m.getClass) throw new OverrideException(s"Implemented method is wrong kind - expected ${m._2.getClass.getSimpleName}, got ${method.getClass.getSimpleName}")
          }

          // Check formal arguments match
          for (t <- traitList) {
            if (!TopTypes.compareFormalArgs(t._1.formalArgs,t._2.formalArgs))
              throw new FormalArgsMismatch(s"Formal args in mixed-in trait ${t._1.name} do not match defined formal args in trait ${t._2.name}")
          }
        }
        case None =>
      }
    }

    def checkTraitsTrait(ponyTrait: Trait) {

    }

    def checkTraitsDeclare(declare: Declare) {

    }

    def findTrait(typename: TypeId): Trait = {
      val t: ModuleMember = module.classes.find(_._1 == typename).getOrElse(throw new TraitNotFoundException(s"Could not find trait $typename in module $filename"))._2

      t match {
        case t: Trait => t
        case _ => throw new ClassExtendsNonTraitType(s"Class extends nontrait type $typename in module $filename")
      }
    }


    new TypedModule(imports, module.classes)
  }


}

object TopTypes {

  def compareFormalArgs(first: FormalArgs, second: FormalArgs): Boolean = {
    if (first.isDefined) {
      second match {
        case Some(sndArgs) => {
          val fstArgs = first.get
          if (sndArgs.length == fstArgs.length) {
            // compare all of the expressions
            // find one that is false
            // check that the option is empty (we didn't find any that didn't match
            // may need to expand comparison of expr for this to work
            // TODO: List[K] ~=~ List[V], provided K or V aren't already defined in scope
            fstArgs.zip(sndArgs).map(tup => tup._1.expr == tup._2.expr).find(p => !p).isEmpty
          } else false
        }
        case None => false
      }
    } else second.isEmpty
  }

}

