package com.github.mt2309.pony.CompilationUnit

import java.io.File
import com.github.mt2309.pony.Parser.PonyParser
import com.github.mt2309.pony.AST.{ModuleMember, Module}
import com.github.mt2309.pony.Typer._

import com.github.mt2309.pony.Common._
import com.github.mt2309.pony.Typer.PreTypedModule
import com.github.mt2309.pony.AST.Module
import com.github.mt2309.pony.Typer.TypedModule
import com.github.mt2309.pony.CodeGen.CodeGenerator
import com.github.mt2309.pony.Loader.Loader

/**
 * User: mthorpe
 * Date: 25/04/2013
 * Time: 23:28
 */
final case class CompilationUnit(absolutePath: String, stage: Int) {

  val fileList = loadDir
  val astList: Seq[(Filename, Option[Module])] = for (file <- fileList) yield file._1 -> PonyParser.parse(file)
  val typedAs: Set[PreTypedModule] = new TopTypes(astList.toSet).topLevelTypes
  val typeIt: Set[TypedModule] = new LowerTypeChecker(typedAs).typeCheck

  private def loadDir: Seq[(Filename, FileContents)] = {
    for (file <- getFilesInDirectory(new File(absolutePath))) yield file.getAbsolutePath -> io.Source.fromFile(file).mkString
  }

  private def getFilesInDirectory(file:File):Seq[File] = {
    if (file.isFile) return Seq(file)

    assert(file.isDirectory)

    for (f <- file.listFiles()) yield f
  }

  def searchType(name: TypeId): Option[TModuleMember] = {
    typeIt.find(t => t.classes.exists(_._1 == name)).flatMap(_.classes.get(name))
  }

  def compile(output: String): Unit = {
    new CodeGenerator(Loader.previouslySeenPaths.values.toIndexedSeq, output).codeGen()
  }
}
