package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.AST._
import com.github.mt2309.pony.Common._
import com.github.mt2309.pony.AST.Type
import com.github.mt2309.pony.AST.Trait
import com.github.mt2309.pony.AST.Actor
import com.github.mt2309.pony.AST.Object
import com.github.mt2309.pony.AST.Declare
import com.github.mt2309.pony.AST.Primitive
//import com.github.mt2309.pony.ImplicitTraits

/**
 * User: mthorpe
 * Date: 25/05/2013
 * Time: 20:58
 */
//final class ITypeChecker(val modules: Set[PreTypedModule]) {
//
//  def typeCheck: Set[ITypedModule] = modules.map(m => pullInTypesModule(m))
//
//  private def pullInTypesModule(pre: PreTypedModule): ITypedModule = {
//    val classes: Map[TypeId, IModuleMember] = pre.classes.map(c => c._1 -> pullInTypes(c._2)(pre.scope, pre.imports, pre.filename))
//
//    val notInThisModule: Set[PreTypedModule] = modules.filterNot(b => b.filename == pre.filename)
//    val more = notInThisModule.map(t => t.scope).flatten
//
//    val extraScope: Map[TypeId, IModuleMember] = {
//      more.filter(_._2.fileName != primitiveFilename).map(t => t._1 -> pullInTypes(t._2)(
//        modules.find(b => b.filename == t._2.fileName).get.scope,
//        modules.find(_.filename == t._2.fileName).get.imports,
//        t._2.fileName
//      )).toMap
//    }
//
//    val scope = new Scope(
//      classes ++ extraScope,
//      pre.imports,
//      Map.empty,
//      new ClassData(None),
//      pre.filename
//    )
//
//    new ITypedModule(pre.imports, classes)(scope)
//  }
//
//  private def pullInTypes(m: ModuleMember)(implicit scope: Map[TypeId, ModuleMember], imports: CompilationUnits, filename: Filename): IModuleMember = {
//    val i = ITypeChecker.lookUp(m)
//
//    val res = i.getOrElse(m match {
//      case d:Declare => new IDeclare(d.name, pullInIs(d.is, isActor = false), d.declareMap)
//      case Actor(name, formal, is, body) => new IActor(name, pullInFormal(formal), pullInIs(is, isActor = true), body)
//      case Object(name, formal, is, body, static) => new IObject(name, pullInFormal(formal), pullInIs(is, isActor = false), body, static)
//      case Primitive(name) => new IPrimitive(name)
//      case Trait(name, formal, is, body) => new ITrait(name, pullInFormal(formal), pullInIs(is, isActor = false), body)
//      case Type(name, of, is) => new IType(name, pullInOf(of), pullInIs(is, isActor = false))
//    })
//
//    if (i.isEmpty) ITypeChecker.store(m, res)
//
//    res
//  }
//
//  private def pullInOf(of: OfType)(implicit scope: Map[TypeId, ModuleMember], imports: CompilationUnits, filename: Filename): IOfType = of match {
//    case c: ConcreteOfType => new IOfType(c.typeList.map(pullInTypeElement))
//    case t: ThisOfType => throw new ThisTypeOutsideClass()(t.pos)
//  }
//
//  private def pullInTypeElement(elem: TypeElement)(implicit scope: Map[TypeId, ModuleMember], imports: CompilationUnits, filename: Filename): ITypeElement = {
//    elem match {
//      case PartialType(tp) => new IPartialType(pullInTypeClass(tp)).setPos(tp.pos)
//      case Lambda(mode, args, result, throws, block) => new ILambda(mode, args, result, throws, block).setPos(mode.pos)
//      case t:TypeClass => pullInTypeClass(t)
//    }
//  }
//
//  private def pullInFormal(f: FormalArgs)(implicit scope: Map[TypeId, ModuleMember], imports: CompilationUnits, filename: Filename): IFormalArgs = {
//    f.map(pullInTypeClass)
//  }
//
//  private def pullInTypeClass(clazz: TypeClass)(implicit scope: Map[TypeId, ModuleMember], imports: CompilationUnits, fName: Filename): ITypeClass = {
//    val modMember: IModuleMember = {
//      val imp = imports.searchType(clazz)
//      imp.getOrElse(pullInTypes(scope.getOrElse(clazz.name, throw new TypeClassNotFoundException(s"$clazz not found in file ${clazz.fileName}")(clazz.pos,new Scope(filename = fName)))))
//    }
//
//    new ITypeClass(modMember, clazz.mode, clazz.formalArgs).setPos(clazz.pos)
//  }
//
//  private def pullInIs(is: Is, isActor: Boolean)(implicit scope: Map[TypeId, ModuleMember], imports: CompilationUnits, filename: Filename): IIs = {
//    val extraTraits = if (isActor) ImplicitTraits.actorTraits else ImplicitTraits.allTraits
//    new IIs(is.list.map(pullInTypeClass) ++ extraTraits).setPos(is.pos)
//  }
//
//}
//
//object ITypeChecker {
//  private var cache: Map[ModuleMember, IModuleMember] = Map.empty
//
//  def lookUp(m: ModuleMember): Option[IModuleMember] = cache.get(m)
//
//  def store(m: ModuleMember, i: IModuleMember): Unit = cache += (m -> i)
//}
