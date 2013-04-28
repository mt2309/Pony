package com.github.mt2309.pony

import scala.collection.mutable.ArrayBuffer
import com.github.mt2309.pony.Error.CompilerError

/**
 * User: mthorpe
 * Date: 27/04/2013
 * Time: 19:30
 */
package object Parser {
  type ErrorList = ArrayBuffer[CompilerError]
}
