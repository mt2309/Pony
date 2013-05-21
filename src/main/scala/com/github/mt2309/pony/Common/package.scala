package com.github.mt2309.pony

import com.github.mt2309.pony.AST.{ModuleMember, Arg}
import com.github.mt2309.pony.CompilationUnit.{UnqualifiedCompilationUnits, QualifiedCompilationUnits}
import com.github.mt2309.pony.Typer.Type

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

  // Formal arguments are (optional) lists of typeIds.
  // Later we could extend this to expressions for value-dependent types
  // But that's a bit beyond the current scope of things.
  type FormalArgs = Option[List[TypeId]]

  // and args are
  type Args = List[Arg]

  type CompilationUnits = (QualifiedCompilationUnits, UnqualifiedCompilationUnits)

  type TypeScope = Map[TypeId, ModuleMember]
  type ValueScope = Map[ID, Type]

}
