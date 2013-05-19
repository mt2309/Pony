package com.github.mt2309.pony.CompilationUnit

import java.io.File
import com.github.mt2309.pony.Parser.PonyParser
import com.github.mt2309.pony.AST.{ModuleMember, Module}
import com.github.mt2309.pony.Typer.{TypedModule, TopTypes}

import com.github.mt2309.pony.Common._

/**
 * User: mthorpe
 * Date: 25/04/2013
 * Time: 23:28
 */
final class CompilationUnit(val absolutePath: String, stage: Int) {

  val fileList = loadDir
  val astList: Seq[(Filename, Option[Module])] = for (file <- fileList) yield file._1 -> PonyParser.parse(file)
  val typedAs: Set[TypedModule] = new TopTypes(astList.toSet).topLevelTypes

  private def loadDir: Seq[(Filename, FileContents)] = {
    for (file <- getFilesInDirectory(new File(absolutePath))) yield (file.getAbsolutePath -> io.Source.fromFile(file).mkString)
  }

  private def getFilesInDirectory(file:File):Seq[File] = {
    if (file.isFile) return Seq(file)

    assert(file.isDirectory)

    for (f <- file.listFiles()) yield f
  }

  def searchType(name: TypeId): Option[ModuleMember] = {
    typedAs.find(t => t.types.find(_._1 == name).isDefined).map(_.types.get(name)).flatten
  }
}
