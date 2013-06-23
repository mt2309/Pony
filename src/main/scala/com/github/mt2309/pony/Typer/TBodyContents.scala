package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.Common._
import com.github.mt2309.pony.CodeGen.CodeGenContext

/**
 * User: mthorpe
 * Date: 19/06/2013
 * Time: 17:26
 */

sealed abstract class TBodyContent(val name: ID, val isAbstract: Boolean = false)(implicit val scope: Scope) extends Typer {
  def ofType: Option[TOfType]
  def mangle: String
  override def codegen(implicit indent: Int, context: CodeGenContext): String
  def header(current: ConcreteClass): String
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

  override def codegen(implicit indent: Int, context: CodeGenContext): String = ???

  override def header(current: ConcreteClass): String = ???

  override def toString = s"TField(id = $id, ofType = $ofType)"
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

  override def codegen(implicit indent: Int, context: CodeGenContext): String = ???

  override def header(current: ConcreteClass): String = ???

  override def toString = s"TDelegate(id = $id, ofType = $ofType)"
}

final case class TConstructor(contents: TMethodContent, throws: Boolean, block: Option[TBlock])(implicit override val scope: Scope) extends TBodyContent(contents.id, block.isEmpty) {

  override def ofType: Option[TOfType] = None

  override def mangle: String = s"${contents.id}_${ArgsHelper.mangle(contents.combinedArgs.args)}"

  override def header(current: ConcreteClass): String = s"pony_clazz * ${current.name}_${contents.id}(variable**);"

  override def codegen(implicit indent: Int, context: CodeGenContext): String = {
    val b = new StringBuilder(s"pony_clazz *\n${context.name}_${contents.id}(variable** args)\n{\n")

    b.appendln(s"pony_clazz * clazz = ${scope.currentClass.name}_construct();")

    for (param <- contents.combinedArgs.args.zipWithIndex) {
      b.appendln(s"${TyperHelper.typeToClass(param._1.ofType)} ${param._1.name} = args[${param._2}]->${TyperHelper.structName(param._1.ofType)};")
    }
    for (variable <- context.variables) {
      b.appendln(s"${TyperHelper.typeToClass(variable._2)} ${variable._1} = lookup_value(clazz, ${variable._1.hashCode.abs})->${TyperHelper.structName(variable._2)};")
    }

    b.append(block.getOrElse(throw new AbstractMethodNotImplemented("")(this.pos)).codegen(indent + 1, context))

    for (variable <- context.variables) {
      b.appendln(s"set_value(clazz, ${TyperHelper.createVariable(variable._2)}(${variable._1}), ${variable._1.hashCode.abs});")
    }

    b.appendln("return clazz;")

    b.append("}\n\n")

    b.mkString
  }

  override def toString = s"TConstructor(contents = $contents, throws = $throws, block = $block)"
}

final case class TAmbient(contents: TMethodContent, throws: Boolean, block: Option[TBlock])(implicit override val scope: Scope) extends TBodyContent(contents.id, block.isEmpty) {
  override def mangle: String = {
    if (contents.combinedArgs.args.size == 0)
      s"${contents.id}_no_arguments"
    else
      s"${contents.id}_${ArgsHelper.mangle(contents.combinedArgs.args)}"
  }

  override def ofType: Option[TOfType] = None

  override def header(current: ConcreteClass): String = ???

  override def codegen(implicit indent: Int, context: CodeGenContext): String = block.getOrElse(throw new AbstractMethodNotImplemented("")(this.pos)).codegen(1, context)

  override def toString = s"TAmbient(contents = $contents, throws = $throws, block = $block)"
}

final case class TFunction(contents: TMethodContent, results: TParams, throws: Boolean, block: Option[TBlock])(implicit override val scope: Scope) extends TBodyContent(contents.id, block.isEmpty) {
  override def mangle: String = {
    if (contents.combinedArgs.args.size == 0)
      s"${contents.id}"
    else
      s"${contents.id}_${ArgsHelper.mangle(contents.combinedArgs.args)}"
  }

  override def ofType: Option[TOfType] = {
    if (results.length == 0) {
      Some(voidOfType)
    } else {
      results.head.ofType
    }
  }

  override def header(current: ConcreteClass): String = s"variable** ${current.name}_${contents.id}(pony_clazz*, variable**);"

  override def codegen(implicit indent: Int, context: CodeGenContext): String = {
    val b = new StringBuilder(s"variable **\n${context.name}_${contents.id}(pony_clazz* this, variable** args)\n{\n")

    for (param <- contents.combinedArgs.args.zipWithIndex) {
      b.appendln(s"${TyperHelper.typeToClass(param._1.ofType)} ${param._1.name} = args[${param._2}]->${TyperHelper.structName(param._1.ofType)};")
    }
    for (result <- results) {
      b.appendln(s"${TyperHelper.typeToClass(result.ofType)} ${result.name} = ${TyperHelper.typeToConstructor(result.ofType)};")
    }
    for (variable <- context.variables) {
      b.appendln(s"${TyperHelper.typeToClass(variable._2)} ${variable._1} = lookup_value(this, ${variable._1.hashCode.abs})->${TyperHelper.structName(variable._2)};")
    }

    b.append(block.getOrElse(throw new AbstractMethodNotImplemented(s"${contents.id} in class ${scope.currentClass.name}")(this.pos)).codegen(indent + 1, context))

    b.append("\n")
    b.appendln("return_label:")
    b.appendln("cleanup:")
    b.appendTo(s"return create_args(${results.length}")

    for (result <- results) {
      b.append(s", ${TyperHelper.createVariable(result.ofType)}(${result.name})")
    }

    b.append(");\n")

    b.append("}\n\n")

    b.mkString
  }

  override def toString = s"TFunction(contents = $contents, results = $results, throws = $throws, block = $block)"
}

final case class TMessage(contents: TMethodContent, block: Option[TBlock])(implicit override val scope: Scope) extends TBodyContent(contents.id, block.isEmpty) {

  override def mangle: String = {
    if (contents.combinedArgs.args.size == 0)
      s"${contents.id}"
    else
      s"${contents.id}_${ArgsHelper.mangle(contents.combinedArgs.args)}"
  }

  override def ofType: Option[TOfType] = None

  override def header(current: ConcreteClass): String = s"variable** ${current.name}_${contents.id}(pony_clazz*, variable**);"

  override def codegen(implicit indent: Int, context: CodeGenContext): String = {
    val b = new StringBuilder(s"variable **\n${context.name}_${contents.id}(pony_clazz* this, variable** args)\n{\n")

    for (param <- contents.combinedArgs.args.zipWithIndex) {
      b.appendln(s"${TyperHelper.typeToClass(param._1.ofType)} ${param._1.name} = args[${param._2}]->${TyperHelper.structName(param._1.ofType)};")
    }
    for (variable <- context.variables) {
      b.appendln(s"${TyperHelper.typeToClass(variable._2)} ${variable._1} = lookup_value(this, ${variable._1.hashCode.abs})->${TyperHelper.structName(variable._2)};")
    }

    b.append(block.getOrElse(throw new AbstractMethodNotImplemented(s"${contents.id} in class ${scope.currentClass.name}")(this.pos)).codegen(indent + 1, context))

    b.append("\n")
    b.appendln("return_label:")
    b.appendln("cleanup:")

    b.appendln("return NULL;")

    b.append("}\n\n")

    b.mkString
  }

  override def toString = s"TMessage(contents = $contents, block = $block)"
}