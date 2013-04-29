package com.github.mt2309.pony

/**
 * User: mthorpe
 * Date: 28/04/2013
 * Time: 23:13
 */
package object AST {

  type TypeId = String
  type ID = String

  type FormalArgs = Option[List[Arg]]
  type Args = List[Arg]

}
