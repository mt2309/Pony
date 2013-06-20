package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.Common._

/**
 * User: mthorpe
 * Date: 19/06/2013
 * Time: 17:26
 */

sealed abstract class TBodyContent(val name: ID, val isAbstract: Boolean = false)(implicit val scope: Scope) extends Typer {
  def mangle: String
  override def codegen(implicit indent: Int): String
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

  override def codegen(implicit indent: Int): String = ???
}

final case class TDelegate(id: ID, ofType: Option[TOfType])(implicit override val scope: Scope) extends TBodyContent(name = id) {
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

  override def codegen(implicit indent: Int) = ???
}

final case class TConstructor(contents: TMethodContent, throws: Boolean, block: Option[TBlock])(implicit override val scope: Scope) extends TBodyContent(contents.id, block.isEmpty) {
  override def mangle: String = s"${contents.id}_${ArgsHelper.mangle(contents.combinedArgs.args)}"

  override def codegen(implicit indent: Int) = block.getOrElse(throw new AbstractMethodNotImplemented("")(this.pos)).codegen(1)
}

final case class TAmbient(contents: TMethodContent, throws: Boolean, block: Option[TBlock])(implicit override val scope: Scope) extends TBodyContent(contents.id, block.isEmpty) {
  override def mangle: String = {
    if (contents.combinedArgs.args.size == 0)
      s"${contents.id}_no_arguments"
    else
      s"${contents.id}_${ArgsHelper.mangle(contents.combinedArgs.args)}"
  }

  override def codegen(implicit indent: Int) = block.getOrElse(throw new AbstractMethodNotImplemented("")(this.pos)).codegen(1)
}

final case class TFunction(contents: TMethodContent, results: TParams, throws: Boolean, block: Option[TBlock])(implicit override val scope: Scope) extends TBodyContent(contents.id, block.isEmpty) {
  override def mangle: String = {
    if (contents.combinedArgs.args.size == 0)
      s"${contents.id}"
    else
      s"${contents.id}_${ArgsHelper.mangle(contents.combinedArgs.args)}"
  }

  override def codegen(implicit indent: Int) = {
    val b = new StringBuilder("{\n")

    for (param <- contents.combinedArgs.args.zipWithIndex) {
      b.appendln(s"${TyperHelper.typeToClass(param._1.ofType)} ${param._1.name} = args[${param._2}];")
    }

    for (result <- results) {
      b.appendln(s"${TyperHelper.typeToClass(result.ofType)} ${result.name} = ${TyperHelper.typeToConstructor(result.ofType)};")
    }

    b.append(block.getOrElse(throw new AbstractMethodNotImplemented(s"${contents.id} in class ${scope.currentClass.name}")(this.pos)).codegen)

    b.appendln("\nreturn_label:")
    b.appendln("cleanup:")
    b.append(s"return create_args(${results.length}")

    for (result <- results) {
      b.append(s", ${TyperHelper.createVariable(result.ofType)}(${result.name})")
    }

    b.append(");")
    b.append("\n}\n\n")

    b.mkString
  }
}

final case class TMessage(contents: TMethodContent, block: Option[TBlock])(implicit override val scope: Scope) extends TBodyContent(contents.id, block.isEmpty) {
  override def mangle: String = {
    if (contents.combinedArgs.args.size == 0)
      s"${contents.id}"
    else
      s"${contents.id}_${ArgsHelper.mangle(contents.combinedArgs.args)}"
  }

  override def codegen(implicit indent: Int) = block.getOrElse(throw new AbstractMethodNotImplemented("")(this.pos)).codegen(1)
}
