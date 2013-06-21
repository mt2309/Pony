package com.github.mt2309.pony

import com.github.mt2309.pony.AST.{TypeClass, Param, Arg}
import com.github.mt2309.pony.CompilationUnit.{UnqualifiedCompilationUnits, QualifiedCompilationUnits}
import Typer.TModuleMember

import scala.language.implicitConversions


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
    def searchType(t: TypeClass): Option[TModuleMember] = t.module match {
      case Some(module) => c._1.lookUpType(t.name, module)(t.pos)
      case None => c._2.lookUpType(t.name)
    }

    def searchType(t: TypeId): Option[TModuleMember] = c._2.lookUpType(t)

    def size: Int = c._1.units.size + c._2.units.size

    override def toString = c._1.toString ++ c._2.toString
  }

  implicit class TabbedBuilder(val builder: StringBuilder) {
    def appendln(s:String)(implicit n: Int) = builder.append(" "*n*2 ++ s ++ "\n")
    def appendTo(s:String)(implicit n: Int) = builder.append(" "*n*2 ++ s)
  }
}
