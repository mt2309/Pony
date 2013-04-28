package com.github.mt2309.pony.Error

/**
 * User: mthorpe
 * Date: 27/04/2013
 * Time: 19:28
 */

final case class CompilerError(filename: String, line: Int, linePos: Int, message: String) {
  override def toString: String = {
    val builder = new StringBuilder("Error at ")
    builder.append(line)
    builder.append(":")
    builder.append(linePos)
    builder.append(":\t" + message)
    builder.toString()
  }
}
