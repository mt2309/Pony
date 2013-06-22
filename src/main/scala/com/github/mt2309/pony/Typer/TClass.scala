package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.Common._
import com.github.mt2309.pony.CodeGen.CodeGenContext

/**
 * User: mthorpe
 * Date: 22/06/2013
 * Time: 18:55
 */
sealed abstract class TModuleMember(implicit val scope: Scope) extends Typer {
  def name: TypeId
  def methods: Map[ID, TBodyContent]
  def isSubType(that: TTypeElement): Boolean
  def variables: Map[ID, Option[TOfType]]
}

final case class EmptyType(override val name: TypeId)(implicit override val scope: Scope) extends TModuleMember with TTypeElement {
  override def isSubType(that: TTypeElement) = this == that

  override def methods = Map.empty

  override def variables = Map.empty

  override val defaultConstructor = "NULL;\n"

  override def codegen(implicit indent: Int, context: CodeGenContext): String = ???

  override def toString = s"EmptyType(name = $name)"
}

final case class TPrimitive(name: TypeId, cTypename: String,  override val defaultConstructor: String)
                           (implicit override val scope: Scope) extends TModuleMember with TTypeElement {

  override def isSubType(that: TTypeElement): Boolean = that match {
    case TPrimitive(pName, _ , _) => pName == name
    case _ => false
  }

  def creation: String = s"create_${name.toLowerCase}_var"

  override def variables = Map.empty

  override def methods = Map("to" -> ImplicitTraits.range)

  override def codegen(implicit indent: Int, context: CodeGenContext): String = ???

  override def equals(that: Any): Boolean = that match {
    case t:TPrimitive => this.name == t.name
    case _ => false
  }

  override def toString = s"Primitive(name = $name)"
}

final case class TDeclare(name: TypeId, is: TIs, declareMap: TDeclareMap)(implicit override val scope: Scope) extends TModuleMember {

  override def methods = is.methods

  override def isSubType(that: TTypeElement): Boolean = ???

  override def variables = Map.empty

  override def codegen(implicit indent: Int, context: CodeGenContext): String = ???

  override def toString = s"TDeclare(name = $name, is = $is, map = $declareMap)"
}

final case class TType(name: TypeId, ofType: TOfType, is: TIs)(implicit override val scope: Scope) extends TModuleMember {
  override def methods = is.methods

  override def isSubType(that: TTypeElement): Boolean = ???

  override def variables = Map.empty

  override def codegen(implicit indent: Int, context: CodeGenContext): String = ???
}

abstract class PonyClass(implicit override val scope: Scope) extends TModuleMember {

  def name: TypeId
  def formalArgs: FormalParams
  def is: TIs
  def typeBody: TTypeBody

  override def isSubType(that: TTypeElement): Boolean = ???

  override def methods = {
    val methods = typeBody.body.filterNot(b => b._2.isInstanceOf[TField] || b._2.isInstanceOf[TField])
    TyperHelper.reduceMethods(List(methods, is.methods))
  }
}

abstract class ConcreteClass(implicit override val scope: Scope) extends PonyClass
{
  def name: TypeId
  def formalArgs: FormalParams
  def is: TIs
  def typeBody: TTypeBody

  def initialiseStatic(implicit indent: Int): String = {
    val b = new StringBuilder(s"static_clazz * create_static_$name(void)\n{\n")

    b.appendln(s"pony_meth * array = NULL;")
    b.appendln(s"unsigned int * id_array = NULL;")

    if (methods.size > 0) {
      b.appendTo(s"array = create_meths(${methods.size}")

      for (meth <- methods) {
        b.append(s", ${name}_${meth._1}")
      }

      b.append(");\n")
      b.appendTo(s"id_array = create_ids(${methods.size}")

      for (meth<- methods) {
        b.append(s", ${meth._1.hashCode.abs}")
      }
      b.append(");\n")
    }

    b.appendln(s"return initialise_static_class(${name}_id, ${methods.size}, id_array, array);\n}\n\n")
    b.mkString
  }

  override def codegen(implicit indent: Int, context: CodeGenContext): String = {
    val b = new StringBuilder(s"pony_clazz * ${name}_construct(void)\n{\n")

    b.appendln("pony_clazz * clazz = malloc(sizeof(pony_clazz));")

    b.appendln(s"variable ** array = NULL;")
    b.appendln(s"unsigned int * id_array = NULL;")

    if (variables.size > 0) {

      for (variable <- variables) {
        b.appendln(s"${TyperHelper.typeToClass(variable._2)} ${variable._1} = ${TyperHelper.typeToConstructor(variable._2)};\n")
      }

      b.appendTo(s"array = create_args(${variables.size}")
      for (variable <- variables) {
        b.append(s", ${TyperHelper.createVariable(variable._2)}(${variable._1})")
      }

      b.append(");\n")
      b.appendTo(s"id_array = create_ids(${variables.size}")
      for (variable <- variables) {
        b.append(s", ${variable._1.hashCode.abs}")
      }
      b.append(");\n")
    }

    b.appendln(s"create_instance_variables(clazz, array, id_array, ${variables.size});")

    b.appendln("return clazz;\n}\n")
    b.mkString
  }

  override def variables = {
    val fields: Map[ID,    Option[TOfType]] = typeBody.body.filter(_._2.isInstanceOf[TField]).map(t =>    t._1 -> t._2.asInstanceOf[TField].ofType)
    val delegates: Map[ID, Option[TOfType]] = typeBody.body.filter(_._2.isInstanceOf[TDelegate]).map(t => t._1 -> t._2.asInstanceOf[TDelegate].ofType)


    TyperHelper.reduceVariables(List(fields, delegates))
  }
}

final case class TActor (name: TypeId, formalArgs: FormalParams, is:TIs, typeBody: TTypeBody)(implicit override val scope: Scope) extends ConcreteClass {
  override def toString = s"TActor(name = $name, formal = $formalArgs, is = $is, typebody = $typeBody)"
}
final case class TObject(name: TypeId, formalArgs: FormalParams, is:TIs, typeBody: TTypeBody)(implicit override val scope: Scope) extends ConcreteClass {
  override def toString = s"TObject(name = $name, formal = $formalArgs, is = $is, typebody = $typeBody)"
}
final case class TTrait (name: TypeId, formalArgs: FormalParams, is:TIs, typeBody: TTypeBody)(implicit override val scope: Scope) extends PonyClass {
  override def variables = Map.empty
  override def codegen(implicit indent: Int, context: CodeGenContext): String = throw new UnsupportedOperationException
  override def toString = s"TTrait(name = $name, formal = $formalArgs, is = $is, typebody = $typeBody)"
}

sealed trait TTypeElement extends Typer {
  def isSubType(that: TTypeElement): Boolean
  def name: String
  def defaultConstructor: String
  override def codegen(implicit indent: Int, context: CodeGenContext): String = ???
}

final case class TPartialType(typeclass: TTypeClass)(implicit val scope: Scope) extends TTypeElement {
  def isSubType(that: TTypeElement): Boolean = that match {
    case t:TPrimitive => false
    case t:TPartialType => this.typeclass.isSubType(t.typeclass)
    case t:TTypeClass => this.typeclass.isSubType(t)
    case t:TLambda => false
    case t:EmptyType => false
  }

  def defaultConstructor = typeclass.defaultConstructor

  override def name = "partial_" ++ typeclass.name

  override def toString = "partial " ++ typeclass.moduleMember.name
}
final case class TTypeClass(moduleMember: TModuleMember, mode: TMode = new TReadOnly()(pScope), formalArgs: TFormalArgs = List.empty)(implicit val scope: Scope) extends TTypeElement {
  def isSubType(that: TTypeElement): Boolean = that match {
    case TPrimitive(name, _, _) => name == moduleMember.name
    case t:TPartialType => this.moduleMember.isSubType(t.typeclass)
    case t:TTypeClass => this.moduleMember.isSubType(t)
    case t:TLambda => false
    case t:EmptyType => false
  }

  def variables: Map[ID, Option[TOfType]] = moduleMember.variables

  def methods: Map[ID, TBodyContent] = moduleMember.methods

  override def toString = moduleMember.name

  override def name = moduleMember.name

  override def defaultConstructor = s"${moduleMember.name}_construct()"
}

final case class TLambda(mode: TMode, args: TArgs, result: TParams, throws: Boolean, block: Option[TBlock])(implicit val scope: Scope) extends TTypeElement {
  def isSubType(that: TTypeElement): Boolean = that match {
    case t:TPrimitive => false
    case t:TPartialType => false
    case t:TTypeClass => false
    case t:EmptyType => false
    case t:TLambda => {
      val arg = t.args.zip(this.args).map(t => t._1 == t._2).reduce(_ && _)
      val res = t.result.zip(this.result).map(t => t._1 == t._2).reduce(_ && _)

      arg && res
    }
  }

  override def defaultConstructor = ???

  override def name = "lambda_" ++ ArgsHelper.mangle(result)

  override def toString = "lambda " ++ args.toString ++ "->" ++ result.toString
}
