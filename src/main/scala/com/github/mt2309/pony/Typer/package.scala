package com.github.mt2309.pony

import com.github.mt2309.pony.Common._
import com.github.mt2309.pony.AST.{Primitive, ModuleMember}

/**
 * User: mthorpe
 * Date: 19/05/2013
 * Time: 14:56
 */
package object Typer {

  val primitiveTypes: Set[ModuleMember] = Set(new Primitive("Int"), new Primitive("UInt"), new Primitive("Char"))

}
