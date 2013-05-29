package com.github.mt2309.pony

import com.github.mt2309.pony.AST.{TypeClass, Param, Arg}
import com.github.mt2309.pony.CompilationUnit.{UnqualifiedCompilationUnits, QualifiedCompilationUnits}
import com.github.mt2309.pony.Typer.IModuleMember

/**
 * User: mthorpe
 * Date: 05/05/2013
 * Time: 20:07
 */

// This is a collection of types we will enforce through the compiler
// Would be pretty cool to assert that `TypeId` always starts with a capital letter
package object Common {

  type TypeId = String
  type ID = String

  // We seem to pass Filename around everywhere, for better or worse right now.
  // Could be have modules carry this info
  type Filename = String
  type FileContents = String

  val primitiveFilename: Filename = "Primitive value"

  // Formal arguments are (optional) lists of typeIds.
  // Later we could extend this to expressions for value-dependent types
  // But that's a bit beyond the current typeScope of things.
  type FormalArgs = List[TypeClass]

  // and args are
  type Args = List[Arg]
  type Params = List[Param]
  type FormalParams = List[TypeId]

  type CompilationUnits = (QualifiedCompilationUnits, UnqualifiedCompilationUnits)

  implicit class ImplicitCompilationOps(val c: CompilationUnits) {
    def searchType(t: TypeClass): Option[IModuleMember] = t.module match {
      case Some(module) => c._1.lookUpType(t.name, module)(t.pos)
      case None => c._2.lookUpType(t.name)
    }
  }

  type ITypeScope = Map[TypeId, IModuleMember]
}
