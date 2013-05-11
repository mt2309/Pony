package com.github.mt2309.pony.CompilationUnit

import java.io.File
import com.github.mt2309.pony.Parser.PonyParser
import com.github.mt2309.pony.AST.Module
import com.github.mt2309.pony.Typer.TopTypes

import com.github.mt2309.pony.Common._

/**
 * User: mthorpe
 * Date: 25/04/2013
 * Time: 23:28
 */
class CompilationUnit(val absolutePath: String, stage: Int) {

  val fileList = loadDir
  val astList: Seq[(Filename, Option[Module])] = for (file <- fileList) yield file._1 -> PonyParser.parse(file)
  val typedAs = new TopTypes(astList.toSet)

  private def loadDir: Seq[(Filename, FileContents)] = {
    for (file <- getFilesInDirectory(new File(absolutePath))) yield (file.getAbsolutePath -> io.Source.fromFile(file).mkString)
  }

  private def getFilesInDirectory(file:File):Seq[File] = {
    if (file.isFile) return Seq(file)

    assert(file.isDirectory)

    for (f <- file.listFiles()) yield f
  }
}
