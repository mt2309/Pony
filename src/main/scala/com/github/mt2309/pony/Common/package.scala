package com.github.mt2309.pony

import com.github.mt2309.pony.AST.Arg

/**
 * User: mthorpe
 * Date: 05/05/2013
 * Time: 20:07
 */
package object Common {

  type TypeId = String
  type ID = String
  type Filename = String
  type FileContents = String

  type FormalArgs = Option[List[Arg]]
  type Args = List[Arg]

}
