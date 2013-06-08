package com.github.mt2309.pony

/**
 * User: mthorpe
 * Date: 28/05/2013
 * Time: 16:36
 */
package object AST {

  final val uVoid = new TypeClass("Void")("Predef")
  final val voidOfType = new ConcreteOfType(Set(uVoid))
}
