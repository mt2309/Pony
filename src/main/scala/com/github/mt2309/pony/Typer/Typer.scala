package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.Common._
import com.github.mt2309.pony.AST._
import util.parsing.input.{NoPosition, Positional}
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

sealed abstract class TModuleMember(val name: TypeId)(implicit val scope: Scope) extends Typer {
  def methods: Map[ID, TBodyContent]
  def isSubType(that: TTypeElement): Boolean
  def variables: Map[ID, Option[TOfType]]
}

final case class EmptyType(override val name: TypeId)(implicit override val scope: Scope) extends TModuleMember(name) with TTypeElement {
  override def isSubType(that: TTypeElement) = this == that

  override def methods = Map.empty

  override def variables = Map.empty

  override val defaultConstructor = "NULL;\n"

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = ???
}

final case class TPrimitive(typename: TypeId, cTypename: String,  override val defaultConstructor: String)
                           (implicit override val scope: Scope) extends TModuleMember(typename) with TTypeElement {

  override def isSubType(that: TTypeElement): Boolean = that match {
    case TPrimitive(pName, _ , _) => pName == typename
    case _ => false
  }

  def creation: String = s"create_${typename.toLowerCase}_var"

  override def variables = Map.empty

  override def methods = Map("to" -> ImplicitTraits.range)

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = ???

  override def equals(that: Any): Boolean = that match {
    case t:TPrimitive => this.typename == t.typename
    case _ => false
  }
}

final case class TDeclare(typename: TypeId, is: TIs, declareMap: TDeclareMap)(implicit override val scope: Scope) extends TModuleMember(typename) {

  override def methods = is.methods

  override def isSubType(that: TTypeElement): Boolean = ???

  override def variables = Map.empty

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = ???
}

final case class TType(n: TypeId, ofType: TOfType, is: TIs)(implicit override val scope: Scope) extends TModuleMember(n) {
  override def methods = is.methods

  override def isSubType(that: TTypeElement): Boolean = ???

  override def variables = Map.empty

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = ???
}

abstract class PonyClass(override val name: TypeId, val formalArgs: FormalParams, val is:TIs, val typeBody: TTypeBody)(implicit override val scope: Scope) extends TModuleMember(name) {

  override def isSubType(that: TTypeElement): Boolean = ???

  override def methods = {
    val methods = typeBody.body.filterNot(b => b._2.isInstanceOf[TField] || b._2.isInstanceOf[TField])
    TyperHelper.reduceMethods(List(methods, is.methods))
  }
}

abstract class ConcreteClass
(override val name: TypeId, override val formalArgs: FormalParams, override val is:TIs, override val typeBody: TTypeBody)(implicit override val scope: Scope) extends PonyClass(name, formalArgs, is, typeBody)
{

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
    val fields: Map[ID, Option[TOfType]] =    typeBody.body.filter(_._2.isInstanceOf[TField]).map(t =>    t._1 -> t._2.asInstanceOf[TField].ofType)
    val delegates: Map[ID, Option[TOfType]] = typeBody.body.filter(_._2.isInstanceOf[TDelegate]).map(t => t._1 -> t._2.asInstanceOf[TDelegate].ofType)


    TyperHelper.reduceVariables(List(fields, delegates))
  }
}

final case class TActor(n: TypeId, f: FormalParams, i:TIs, t: TTypeBody)(implicit override val scope: Scope)   extends ConcreteClass(n,f,i,t)
final case class TObject(n: TypeId, f: FormalParams, i:TIs, t: TTypeBody)(implicit override val scope: Scope)  extends ConcreteClass(n,f,i,t)
final case class TTrait(n: TypeId, f: FormalParams, i:TIs, t: TTypeBody)(implicit override val scope: Scope)   extends PonyClass(n,f,i,t) {
  override def variables = Map.empty
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = ???
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

    println(s"this = $this")
    println(s"that = $that")

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

  def intersection(that: TOfType): TOfType = new TOfType(typeList.intersect(that.typeList))

}

final case class TTypeBody(body: Map[ID,TBodyContent])(implicit val scope: Scope) extends Typer {
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = ???
}



final case class TMethodContent(mode: TMode, id:ID, combinedArgs: TCombinedArgs)(implicit val scope: Scope) extends Typer {
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = ???
}

final case class TExpr(unary: TUnary, operator: List[(Operator, TUnary)])(implicit val scope: Scope) extends Typer {

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

  def extractOfType(implicit scope: Scope): Option[TOfType] = {
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
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = ???
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

sealed abstract class TUnary(unaryOps: List[UnaryOp])(implicit val scope: Scope) extends Typer {
  def extractOfType: Option[TOfType]
}

final case class TUnaryCommand(un: List[UnaryOp], command: TCommand)(implicit override val scope: Scope) extends TUnary(un) {
  override def extractOfType = command.extractOfType

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = {
    val b = new StringBuilder

    for (op <- un)
      b.append(op.codeGen)

    b.append(" " ++ command.codegen)

    b.mkString
  }
}

final case class TUnaryLambda(un: List[UnaryOp], lambda: TLambda)(implicit override val scope: Scope) extends TUnary(un) {
  override def extractOfType = Some(new TOfType(Set(lambda)))

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = ???
}

final case class TCommand(first: TFirstCommand, second: Option[TSecondCommand])(implicit val scope: Scope) extends Typer {
  def extractOfType: Option[TOfType] = first.extractOfType

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = s"${first.codegen}${if (second.isDefined) second.get.codegen else ""}"

  def constructor: String = ""
}

sealed abstract class TFirstCommand extends Typer {
  def extractOfType: Option[TOfType]
}

final case class TCommandExpr(expr: TExpr)(implicit val scope: Scope) extends TFirstCommand with Typer {
  override def extractOfType: Option[TOfType] = expr.extractOfType

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = ???
}

final case class TCommandArgs(args: List[TArg])(implicit val scope: Scope) extends TFirstCommand with Typer {
  override def extractOfType: Option[TOfType] = ???

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = ???
}

sealed abstract class TAtom extends TFirstCommand with Typer

final class TThis(implicit val scope: Scope) extends TAtom {
  override def extractOfType = None

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = "this"
}

final class  TTrue(implicit val scope: Scope) extends TAtom {
  override def extractOfType = Some(boolOfType)

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = "true"
}

final class  TFalse(implicit val scope: Scope) extends TAtom {
  override def extractOfType = Some(boolOfType)

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = "false"
}

final case class TPonyInt(i: Int)(implicit val scope: Scope) extends TAtom with Typer {
  override def extractOfType = Some(intOfType)

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = i.toString
}

final case class TPonyDouble(d: Double)(implicit val scope: Scope) extends TAtom with Typer {
  override def extractOfType = Some(doubleOfType)

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = d.toString
}

final case class TPonyString(s: String)(implicit val scope: Scope) extends TAtom with Typer {
  override def extractOfType = Some(stringOfType)

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = "\"" ++ s ++ "\""
}

final case class TPonyID(i: ID)(implicit val scope: Scope) extends TAtom with Typer {
  override def extractOfType = scope.searchID(i)(this.pos)

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = i
}

final case class TPonyTypeId(t: TypeId)(implicit val scope: Scope) extends TAtom with Typer {
  override def extractOfType = {
    val search = scope.search(t)(this.pos)

    search match {
      case t: TPrimitive => Some(new TOfType(Set(t)))
      case t: TModuleMember => Some(new TOfType(Set(new TTypeClass(t))))
    }
  }

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = t
}

sealed abstract class TSecondCommand(implicit val scope: Scope) extends Typer {
  def extractOfType(fst: Option[TOfType]): Option[TOfType]
}

final case class TSecondCommandArgs(args: TArgs)(implicit override val scope: Scope) extends TSecondCommand with Typer {
  override def extractOfType(fst: Option[TOfType]): Option[TOfType] = ???

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = ???
}

final case class TCommandCall(id: TBodyContent, formalArgs: TFormalArgs, args: TArgs)(implicit override val scope: Scope) extends TSecondCommand with Typer {
  override def extractOfType(fst: Option[TOfType]): Option[TOfType] = id.ofType

  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = {
    s"${id.scope.currentClass.name}_${id.name}(this, ${ArgsHelper.codeGen(args)})"
  }
}
