package com.github.mt2309.pony

import collection.mutable.ArrayBuffer
import Error.CompilerError

/**
 * User: mthorpe
 * Date: 27/04/2013
 * Time: 19:30
 */
package object Parser {
  type ErrorList = ArrayBuffer[CompilerError]
}
