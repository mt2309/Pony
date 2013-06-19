package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.Common._

/**
 * User: mthorpe
 * Date: 19/06/2013
 * Time: 17:26
 */
final case class TBlock(contents:List[TBlockContent], catchBlock: Option[TBlock], alwaysBlock: Option[TBlock])(implicit val scope: Scope) extends TBlockContent with Typer {
  def codeGen: String = {
    val b = new StringBuilder("{\n")

    for (c <- contents) {
      b.append(c.codeGen)
      b.append("\n")
    }

    b.append("}\n")


    b.mkString
  }
}

sealed abstract class TBodyContent(val name: ID, val isAbstract: Boolean = false)(implicit val scope: Scope) extends Typer  {
  def mangle: String
  def codeGen: String
}

final case class TField(id: ID, ofType: Option[TOfType], expr: Option[TExpr])(implicit override val scope: Scope) extends TBodyContent(id, expr.isEmpty) {
  override def mangle: String = {
    if (ofType.isDefined) {
      if (ofType.get.size == 0)
        s"${id}_no_arguments"
      else
        s"${id}_${ofType.get.mangle}"
    }
    else
      s"${id}_${scope.currentClass.name}"
  }

  override def codeGen: String = ???
}

final case class TDelegate(id: ID, ofType: Option[TOfType])(implicit override val scope: Scope) extends TBodyContent(name = id)  {
  override def mangle: String = {
    if (ofType.isDefined) {
      if (ofType.get.size == 0)
        s"$id"
      else
        s"${id}_${ofType.get.mangle}"
    }
    else
      s"${id}_${scope.currentClass.name}"
  }

  override def codeGen: String = ???
}

final case class TConstructor(contents: TMethodContent, throws: Boolean, block: Option[TBlock])(implicit override val scope: Scope) extends TBodyContent(contents.id, block.isEmpty) {
  override def mangle: String = s"${contents.id}_${ArgsHelper.mangle(contents.combinedArgs.args)}"

  override def codeGen: String = block.getOrElse(throw new AbstractMethodNotImplemented("")(this.pos)).codeGen
}

final case class TAmbient(contents: TMethodContent, throws: Boolean, block: Option[TBlock])(implicit override val scope: Scope) extends TBodyContent(contents.id, block.isEmpty) {
  override def mangle: String = {
    if (contents.combinedArgs.args.size == 0)
      s"${contents.id}_no_arguments"
    else
      s"${contents.id}_${ArgsHelper.mangle(contents.combinedArgs.args)}"
  }

  override def codeGen: String = block.getOrElse(throw new AbstractMethodNotImplemented("")(this.pos)).codeGen
}

final case class TFunction(contents: TMethodContent, results: TParams, throws: Boolean, block: Option[TBlock])(implicit override val scope: Scope) extends TBodyContent(contents.id, block.isEmpty) {
  override def mangle: String = {
    if (contents.combinedArgs.args.size == 0)
      s"${contents.id}"
    else
      s"${contents.id}_${ArgsHelper.mangle(contents.combinedArgs.args)}"
  }

  override def codeGen: String = {
    val b = new StringBuilder

    for (result <- results) {
      b.append(TyperHelper.typeToClass(result.ofType))
      b.append(" ")
      b.append(result.name)
      b.append(" = ")
      b.append(TyperHelper.typeToConstructor(result.ofType))
      b.append("\n")
    }

    b.append(block.getOrElse(throw new AbstractMethodNotImplemented(s"${contents.id} in class ${scope.currentClass.name}")(this.pos)).codeGen)

    if (results.length == 0) b.append("return NULL;")

    b.mkString
  }
}

final case class TMessage(contents: TMethodContent, block: Option[TBlock])(implicit override val scope: Scope) extends TBodyContent(contents.id, block.isEmpty)  {
  override def mangle: String = {
    if (contents.combinedArgs.args.size == 0)
      s"${contents.id}"
    else
      s"${contents.id}_${ArgsHelper.mangle(contents.combinedArgs.args)}"
  }

  override def codeGen: String = block.getOrElse(throw new AbstractMethodNotImplemented("")(this.pos)).codeGen
}
