package com.github.mt2309.pony

import com.github.mt2309.pony.AST.{ModuleMember, Primitive}

/**
 * User: mthorpe
 * Date: 19/05/2013
 * Time: 14:56
 */
package object Typer {

  val void: Primitive = new Primitive("Void")

  val primitiveTypes: Set[ModuleMember] = Set(new Primitive("Int"), new Primitive("UInt"), new Primitive("Char"))

  // code duplication :(
  val tVoid = new TPrimitive("Void")
  val tPrimitiveTypes: Set[TModuleMember] = Set(new TPrimitive("Int"), new TPrimitive("UInt"), new TPrimitive("Char"))

}
