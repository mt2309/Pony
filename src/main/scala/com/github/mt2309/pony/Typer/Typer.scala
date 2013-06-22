package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.Common._
import com.github.mt2309.pony.AST._
import util.parsing.input.Positional
import annotation.tailrec

/**
 * User: mthorpe
 * Date: 30/04/2013
 * Time: 00:57
 */


trait Typer extends NotNull with Positional {
  def scope: Scope
  def codegen(implicit indent: Int, currentClazz: ConcreteClass): String
}

final case class PreTypedModule(imports: CompilationUnits, classes: Map[TypeId, ModuleMember])(implicit val scope: Map[TypeId, ModuleMember], val filename: Filename) extends NotNull with Positional

final case class TypedModule(imports:CompilationUnits, classes: Map[TypeId, TModuleMember])(implicit val scope: Scope) extends Typer {
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = ???
}

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

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = ???

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

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = ???

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

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = ???

  override def toString = s"TDeclare(name = $name, is = $is, map = $declareMap)"
}

final case class TType(name: TypeId, ofType: TOfType, is: TIs)(implicit override val scope: Scope) extends TModuleMember {
  override def methods = is.methods

  override def isSubType(that: TTypeElement): Boolean = ???

  override def variables = Map.empty

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = ???
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

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = {
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

final case class TActor (name: TypeId, formalArgs: FormalParams, is:TIs, typeBody: TTypeBody)(implicit override val scope: Scope) extends ConcreteClass
final case class TObject(name: TypeId, formalArgs: FormalParams, is:TIs, typeBody: TTypeBody)(implicit override val scope: Scope) extends ConcreteClass
final case class TTrait (name: TypeId, formalArgs: FormalParams, is:TIs, typeBody: TTypeBody)(implicit override val scope: Scope) extends PonyClass {
  override def variables = Map.empty
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = throw new UnsupportedOperationException
}

final case class TParam(name: ID, ofType: Option[TOfType])(implicit val scope: Scope)

final case class TCombinedArgs(formalArgs: FormalParams, args: TParams)(implicit val scope: Scope) extends Typer {
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = ???
}
final case class TArg(expr: Option[TExpr], ofType: Option[TOfType], assign: Option[TExpr])(implicit val scope: Scope) extends Typer {

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = expr.map(_.codegen).getOrElse("") ++ assign.map(_.codegen).getOrElse("")
}

sealed trait TTypeElement extends Typer {
  def isSubType(that: TTypeElement): Boolean
  def name: String
  def defaultConstructor: String
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = ???
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

sealed abstract class TMode(implicit val scope: Scope)              extends Typer {
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = ???
}
final case class TReadOnly(implicit override val scope: Scope)      extends TMode
final case class TImmutable(implicit override val scope: Scope)     extends TMode
final case class TMutable(implicit override val scope: Scope)       extends TMode
final case class TUnique(implicit override val scope: Scope)        extends TMode
final case class TModeExpr(expr: TExpr)(implicit override val scope: Scope) extends TMode


final case class TIs(list: List[TTypeClass])(implicit val scope: Scope) extends Typer {
  def variables: Map[ID, Option[TOfType]] = {
    val vars = for (tClass <- list) yield tClass.variables

    TyperHelper.reduceVariables(vars)
  }

  def methods: Map[ID, TBodyContent] = {
    val bodies = for (tClass <- list) yield tClass.methods

    TyperHelper.reduceMethods(bodies)
  }

  def isSubType(that: TTypeElement): Boolean = ???

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = ???
}

final case class TDeclareMap(map: List[TPonyMap])(implicit val scope: Scope) extends Typer {
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = ???
}

final case class TPonyMap(from: TBodyContent, to: ID)(implicit val scope: Scope) extends Typer {
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = ???
}

final case class TOfType(typeList: Set[TTypeElement])(implicit val scope: Scope) extends Typer {

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = {
    if (this.isSubType(Some(primTOfType)))
      "0;\n"
    else
      typeList.head.defaultConstructor ++ ";\n"
  }

  def isPrimitive: Boolean = isSubType(Some(primTOfType))

  def isEmpty: Boolean = typeList.exists(_.isInstanceOf[EmptyType])


  def maximalType: TPrimitive = {
    assert(isPrimitive)

    // oh god shoot me this is gonna be rough
    if (this.isSubType(Some(numericOfType))) {
      if (this.isSubType(Some(doubleOfType)))
        pDouble
      else if (this.isSubType(Some(intOfType)))
        pInt
      else
        pUInt
    }
    else {
      if (this.isSubType(Some(boolOfType))) {
        bool
      }
      else if (this.isSubType(Some(stringOfType)))
        pString
      else
        pChar
    }
  }

  def isSubType(that: Option[TOfType]): Boolean = {

    if (that.isDefined) {
      (for (t <- this.typeList) yield {
        (for (tt <- that.get.typeList) yield tt == t || t.isSubType(tt)).reduce(_ || _)
      }).reduce(_ && _)
    } else {
      // this.type
      (for (t <- this.typeList) yield scope.isList.exists(_.isSubType(t))).reduce(_ && _)
    }
  }

  def mangle = {
    val b = new StringBuilder

    for (t <- typeList) b.append(s"${t.name}_")

    b.toString
  }

  def size = typeList.size

  def intersection(that: TOfType): TOfType = {
    val list = typeList.intersect(that.typeList)

    if (list.isEmpty) {
      doubleOfType
    }
    else {
      new TOfType(list)
    }
  }

}

final case class TTypeBody(body: Map[ID,TBodyContent])(implicit val scope: Scope) extends Typer {
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = ???
}



final case class TMethodContent(mode: TMode, id:ID, combinedArgs: TCombinedArgs)(implicit val scope: Scope) extends Typer {
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = throw new UnsupportedOperationException
}

final case class TExpr(unary: TUnary, operator: List[(Operator, TUnary)])(implicit val scope: Scope) extends Typer {

  def tail(implicit indent: Int, currentClazz: ConcreteClass): String = unary.tail

  def isSimple: Boolean = {
    if (operator.isEmpty) {
      unary.isSimple
    }
    else {
      true
    }
  }

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = {
    val u = unary.codegen
    if (operator.isEmpty)
      u
    else {
      val b = new StringBuilder(u)

      for (o <- operator) {
        b.append(s" ${o._1.codeGen} ${o._2.codegen}")
      }

      b.mkString
    }
  }

  def ofType(implicit scope: Scope): Option[TOfType] = {
    val unaryOfType = unary.extractOfType
    if (operator.isEmpty)
      unaryOfType
    else
      opList(unaryOfType, operator)
  }

  @tailrec
  private def opList(of: Option[TOfType], list: List[(Operator, TUnary)]): Option[TOfType] = list match {
    case x :: xs => {
      val rhsType = x._2.extractOfType.getOrElse(throw new TypeMismatch(primTOfType.toString, "this")(x._1.pos, scope))
      val lhsType = of.getOrElse(throw new TypeMismatch(primTOfType.toString, "this")(x._1.pos, scope))
      x._1 match {
        case t: NumericOp => {
          if (lhsType.isSubType(Some(numericOfType)) && rhsType.isSubType(Some(numericOfType)))
            opList(Some(rhsType.intersection(lhsType)), xs)
          else
            throw new TypeMismatch(numericOfType.toString, rhsType.toString)(x._1.pos, scope)
        }

        case t: BooleanOp => {
          if (lhsType.isSubType(Some(numericOfType)) && rhsType.isSubType(Some(numericOfType)))
            opList(Some(boolOfType), xs)
          else
            throw new TypeMismatch(boolOfType.toString, rhsType.toString)(x._1.pos, scope)
        }

        case t: NumericBooleanOp => {
          if (lhsType.isSubType(Some(primTOfType)) && rhsType.isSubType(Some(primTOfType)))
            opList(Some(boolOfType), xs)
          else
            throw new TypeMismatch(primTOfType.toString, rhsType.toString)(x._1.pos, scope)
        }

        case t: TypeOp => ???
      }
    }

    case Nil => of
  }
}

final case class TCaseBlock(c: Option[TCaseSubBlock], block: TBlock)(implicit val scope: Scope) extends Typer {
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = ???
}
abstract class TCaseSubBlock(implicit val scope: Scope) extends Typer {
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = ???
}
final case class TCaseIf(expr: TExpr)(implicit override val scope: Scope) extends TCaseSubBlock {
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = {
    val b = new StringBuilder

    b.appendln(s"if (${expr.codegen})")

    b.mkString
  }
}
final case class TCaseVarList(varList: List[TCaseVar])(implicit override val scope: Scope) extends TCaseSubBlock {
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = ???
}

final case class TCaseVar(expr: Option[TExpr], forVar: TForVar)(implicit val scope: Scope) extends Typer {
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = ???
}
final case class TForVar(id: ID, ofType: Option[TOfType])(implicit val scope: Scope) extends Typer {
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = ???
}


sealed abstract class TLValue(implicit val scope: Scope) extends Typer {
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String
  def constructor(implicit indent: Int, currentClazz: ConcreteClass): String
  def ofType: Option[TOfType]
}

final case class TLValueVar(nVar: TVarDec)(implicit override val scope: Scope) extends TLValue {
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = nVar.codegen

  override def constructor(implicit indent: Int, currentClazz: ConcreteClass) = nVar.constructor

  override def ofType: Option[TOfType] = nVar.ofType
}
final case class TLValueCommand(command: TCommand)(implicit override val scope: Scope) extends TLValue {
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = command.codegen

  override def constructor(implicit indent: Int, currentClazz: ConcreteClass) = command.constructor

  override def ofType: Option[TOfType] = command.extractOfType
}

sealed abstract class TUnary(implicit val scope: Scope) extends Typer {
  def extractOfType: Option[TOfType]
  def isSimple: Boolean
  def tail(implicit indent: Int, currentClazz: ConcreteClass): String
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String

  def unaryOps: List[UnaryOp]
}

final case class TUnaryCommand(unaryOps: List[UnaryOp], command: TCommand)(implicit override val scope: Scope) extends TUnary {
  override def extractOfType = command.extractOfType

  override def isSimple: Boolean = command.isSimple

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = {
    val b = new StringBuilder

    for (op <- unaryOps)
      b.append(op.codeGen)

    b.append(command.codegen)

    b.mkString
  }

  override def tail(implicit indent: Int, currentClazz: ConcreteClass): String = command.tail
}

final case class TUnaryLambda(unaryOps: List[UnaryOp], lambda: TLambda)(implicit override val scope: Scope) extends TUnary {
  override def extractOfType = Some(new TOfType(Set(lambda)))

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = ???

  override def tail(implicit indent: Int, currentClazz: ConcreteClass): String = throw new UnsupportedOperationException

  override def isSimple: Boolean = false
}

final case class TCommand(first: TFirstCommand, second: Option[TSecondCommand])(implicit val scope: Scope) extends Typer {
  def extractOfType: Option[TOfType] = first.extractOfType

  def isSimple: Boolean = {
    if (second.isDefined) {
      println(second)
      false
    } else {
      first.isSimple
    }
  }

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = {
    val b = new StringBuilder

    b.append(first.codegen)

    second.map(m => b.append("(" ++ m.codegen ++ ")"))

    b.mkString
  }

  def constructor: String = ""

  def tail(implicit indent: Int, currentClazz: ConcreteClass) = first.tail

  override def toString = s"TCommand(first = $first, second = $second)"
}

sealed abstract class TFirstCommand extends Typer {
  def extractOfType: Option[TOfType]
  def tail(implicit indent: Int, currentClazz: ConcreteClass): String
  def isSimple: Boolean
}

final case class TCommandExpr(expr: TExpr)(implicit val scope: Scope) extends TFirstCommand with Typer {
  override def extractOfType: Option[TOfType] = expr.ofType

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = "(" ++ expr.codegen ++ ")"

  override def tail(implicit indent: Int, currentClazz: ConcreteClass): String = expr.tail

  override def isSimple: Boolean = false

  override def toString = s"TCommandExpr(expr = $expr)"
}

final case class TCommandArgs(args: List[TArg])(implicit val scope: Scope) extends TFirstCommand with Typer {
  override def extractOfType: Option[TOfType] = ???

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = ???

  override def tail(implicit indent: Int, currentClazz: ConcreteClass): String = args.head.expr.get.tail

  override def isSimple: Boolean = false

  override def toString = s"TCommandArgs(args = $args)"
}

sealed abstract class TAtom extends TFirstCommand with Typer {
  override def tail(implicit indent: Int, currentClazz: ConcreteClass): String = codegen
  override def isSimple: Boolean
}

final class TThis(implicit val scope: Scope) extends TAtom {
  override def extractOfType = None

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = "this"

  override def isSimple: Boolean = false

  override def toString = "TThis"
}

final class TTrue(implicit val scope: Scope) extends TAtom {
  override def extractOfType = Some(boolOfType)

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = "true"

  override def isSimple: Boolean = true

  override def toString = "TTrue"
}

final class  TFalse(implicit val scope: Scope) extends TAtom {
  override def extractOfType = Some(boolOfType)

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = "false"

  override def isSimple: Boolean = true

  override def toString = "TFalse"
}

final case class TPonyInt(i: Int)(implicit val scope: Scope) extends TAtom with Typer {
  override def extractOfType = Some(intOfType)

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = i.toString

  override def isSimple: Boolean = true

  override def toString = s"Int($i)"
}

final case class TPonyDouble(d: Double)(implicit val scope: Scope) extends TAtom with Typer {
  override def extractOfType = Some(doubleOfType)

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = d.toString

  override def isSimple: Boolean = true

  override def toString = s"Double($d)"
}

final case class TPonyString(s: String)(implicit val scope: Scope) extends TAtom with Typer {
  override def extractOfType = Some(stringOfType)

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = "\"" ++ s ++ "\""

  override def isSimple: Boolean = true

  override def toString = s"String($s)"
}

final case class TPonyID(i: ID)(implicit val scope: Scope) extends TAtom with Typer {
  override def extractOfType = scope.searchID(i)(this.pos)

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = {
    scope.findID(i) match {
      case Some(found) => found match {
        case v:Var => i
        case m:Meth => s"${currentClazz.name}_$i"
      }
      case None => ???
    }
  }

  override def isSimple: Boolean = {
    scope.findID(i) match {
      case Some(found) => found match {
        case v:Var => true
        case m:Meth => false
      }
      case None => ???
    }
  }

}

final case class TPonyTypeId(t: TypeId)(implicit val scope: Scope) extends TAtom with Typer {
  override def extractOfType = {
    val search = scope.search(t)(this.pos)

    search match {
      case t: TPrimitive => Some(new TOfType(Set(t)))
      case t: TModuleMember => Some(new TOfType(Set(new TTypeClass(t))))
    }
  }

  override def isSimple: Boolean = false

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = ""
}

sealed abstract class TSecondCommand(implicit val scope: Scope) extends Typer {
  def extractOfType(fst: Option[TOfType]): Option[TOfType]
  def tail(implicit indent: Int, currentClazz: ConcreteClass): String
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String
}

final case class TSecondCommandArgs(args: TArgs)(implicit override val scope: Scope) extends TSecondCommand with Typer {
  override def extractOfType(fst: Option[TOfType]): Option[TOfType] = ???

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = {
    val b = new StringBuilder(s"this, create_args(${args.length}")

    if (args.isEmpty) {
      b.append(", NULL")
    }
    else {
      for (arg <- args) {
        b.append(s", ${TyperHelper.createVariable(arg.ofType)}(${arg.expr.get.codegen})")
      }
    }

    b.append(")")

    b.mkString
  }

  def tail(implicit indent: Int, currentClazz: ConcreteClass) = args.head.expr.get.codegen

  override def toString = s"TSecondCommandArgs(args = $args)"
}

final case class TCommandCall(id: TBodyContent, formalArgs: TFormalArgs, args: TArgs)(implicit override val scope: Scope) extends TSecondCommand with Typer {
  override def extractOfType(fst: Option[TOfType]): Option[TOfType] = id.ofType

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = {
    val b = new StringBuilder(s"${id.scope.currentClass.name}_${id.name}(this, create_args(${args.length}")

    if (args.isEmpty) {
      b.append(", NULL")
    }
    else {
      for (arg <- args) {
        val e = arg.expr.get
        b.append(s", ${TyperHelper.createVariable(e.ofType)}(${e.codegen})")
      }
    }

    b.append(")")

    b.mkString
  }

  override def toString = s"TCommandCall(id = $id, formalArgs = $formalArgs, args = $args)"

  def tail(implicit indent: Int, currentClazz: ConcreteClass) = throw new UnsupportedOperationException
}
