package com.github.mt2309.pony.Loader

import com.github.mt2309.pony.CompilationUnit.CompilationUnit
import java.io.{IOException, File}

/**
 * User: mthorpe
 * Date: 25/04/2013
 * Time: 23:24
 */
object Loader {

  var previouslySeenPaths:Map[String, CompilationUnit] = Map.empty

  var stage: Int = 3

  def load(s: Int, path: String): CompilationUnit = {

    stage = s

    val dir = new File(path)
    privateLoad(dir)
  }

  def load(currentPath: String, relativePath: String): CompilationUnit = {
    val dir = new File(createPath(currentPath, relativePath))

    privateLoad(dir)
  }

  private def privateLoad(dir: File): CompilationUnit = {
    if (!dir.isDirectory) {
      if (dir.isFile) new CompilationUnit(dir.getAbsolutePath, stage)
      else throw new IOException(dir.getAbsolutePath + " not found")
    }

    previouslySeenPaths.get(dir.getAbsolutePath) match {
      case Some(x) => x
      case None => {
        val unit = new CompilationUnit(dir.getAbsolutePath, stage)
        previouslySeenPaths += dir.getAbsolutePath -> unit
        unit
      }
    }
  }

  private def createPath(currentPath: String, relativePath: String): String = {
    if (relativePath.length == 0) return relativePath

    relativePath.charAt(0) match {
      case '/' => relativePath
      case _ => currentPath + "/" + relativePath
    }
  }
}
