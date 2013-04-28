package com.github.mt2309.pony.CompilationUnit

import java.io.File
import com.github.mt2309.pony.Parser.PonyParser
import scala.util.parsing.combinator.Parsers

/**
 * User: mthorpe
 * Date: 25/04/2013
 * Time: 23:28
 */
class CompilationUnit(val absolutePath: String, stage: Int) {

  type Filename = String
  type FileContents = String

  val fileList = loadDir
//  val astList: Seq[AST] = for (file <- fileList) yield file


  def buildUnit() {
    for (file <- loadDir) PonyParser.parse(file._2)
  }

  private def loadDir: Seq[(Filename, FileContents)] = {
    for (file <- getFilesInDirectory(new File(absolutePath))) yield (file.getAbsolutePath -> io.Source.fromFile(file).mkString)
  }

  private def getFilesInDirectory(file:File):Seq[File] = {
    if (file.isFile) return Seq(file)

    assert(file.isDirectory)

    for (f <- file.listFiles()) yield f
  }

}
