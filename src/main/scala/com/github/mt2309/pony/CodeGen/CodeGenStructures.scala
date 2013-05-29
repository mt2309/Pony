package com.github.mt2309.pony.CodeGen

import com.github.mt2309.pony.Common.{TypeId, FileContents}
/**
 * User: mthorpe
 * Date: 29/05/2013
 * Time: 16:57
 */

final case class CompleteFile(header: Header, source: SourceFile)

final case class Header(className: TypeId, text: FileContents)
final case class SourceFile(header: Header, text: FileContents)