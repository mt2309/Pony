package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.Common._
import annotation.tailrec
import com.github.mt2309.pony.CodeGen.CodeGenContext

/**
 * User: mthorpe
 * Date: 11/06/2013
 * Time: 03:38
 */
private object TyperHelper {

  def isUnique(opt: Option[TOfType]): Boolean = opt match {
    case Some(of) => of.isUnique
    case None => false
  }

  def mode(opt: Option[TOfType])(implicit context: CodeGenContext): TMode = opt match {
    case Some(of) => of.mode
    case None => context.mode
  }

  def subType(left: Option[TOfType], that: Option[TOfType]): Boolean = left match {
    case Some(of) => of.isSubType(that)
    case None => that match {
      case Some(ofThat) => ???
      case None => true
    }
  }

  def sendable(opt: Option[TOfType]): Boolean = opt match {
    case Some(of) => of.isSendable
    case None => false // can we send "this" to another actor?
  }

  def extractFrom(atom: TAtom)(implicit scope: Scope, context: CodeGenContext): String = atom match {
    case TPonyInt(i) => throw new UnsupportedOperationException
    case t: TBoolean => throw new UnsupportedOperationException
    case TPonyDouble(d) => throw new UnsupportedOperationException
    case TPonyString(str) => throw new UnsupportedOperationException
    case TPonyID(id) => {
      scope.findID(id) match {
        case Some(found) => found match {
          case v: Var => ""
          case m: Meth => scope.searchMethod(id).cResult
        }
        case None => ""
      }
    }
    case TPonyTypeId(id) => throw new UnsupportedOperationException
    case t: TThis => throw new UnsupportedOperationException
  }

  def typeToClass(opt: Option[TOfType]): String = opt match {
    case Some(x) => {
      if (x.isPrimitive) {
        x.maximalType.cTypename
      }
      else {
        "pony_clazz *"
      }
    }
    case None => {
      "pony_clazz *"
    }
  }

  def isPrimitive(opt: Option[TOfType]): Boolean = opt.exists(_.isPrimitive)

  def isSingleRes(opt: Option[TOfType]): Boolean = opt match {
    case Some(x) => { true

    }
    case None => true
  }

  def structName(opt: Option[TOfType]): String = opt match {
    case Some(x) => {
      if (x.isPrimitive) {
        x.maximalType.cTypename ++ "_value"
      }
      else {
        "clazz_value"
      }
    }
    case None => {
      "clazz_value"
    }
  }

  def createVariable(opt: Option[TOfType]): String = opt match {
    case Some(of) => {
      if (of.isPrimitive) {
        of.maximalType.creation
      }
      else {
        "create_clazz_var"
      }
    }

    case None => "create_clazz_var"
  }

  def createVariable(expr: TExpr): String = {
    createVariable(expr.ofType(expr.scope))
  }


  def typeToConstructor(opt: Option[TOfType])(implicit context: CodeGenContext): String = opt match {
    case Some(x) => {
      if (x.isPrimitive) {
        x.maximalType.defaultConstructor
      }
      else if (x.isEmpty) {
        "NULL"
      }
      else {
        x.codegen(0, context)
      }
    }
    case None => "NULL"
  }

  def reduceVariables(list: List[Map[ID, Option[TOfType]]]): Map[ID, Option[TOfType]] = reduceVariablesHelper(list, Map.empty)
  def reduceMethods(list: List[Map[ID, TBodyContent]]): Map[ID, TBodyContent] = reduceMethodsHelper(list, Map.empty)

  @tailrec
  private def reduceMethodsHelper(list: List[Map[ID, TBodyContent]], map: Map[ID, TBodyContent]): Map[ID, TBodyContent] = list match {
    case x :: xs => {
      var m = map
      for (meth <- x) {
        if (!m.contains(meth._1) || m(meth._1).isAbstract) m += meth
      }

      reduceMethodsHelper(xs , m)
    }

    case Nil => map
  }

  @tailrec
  private def reduceVariablesHelper(list: List[Map[ID, Option[TOfType]]], map: Map[ID, Option[TOfType]]): Map[ID, Option[TOfType]] = list match {
    case x :: xs => {
      var m = map
      for (varD <- x) {
        if (!m.contains(varD._1)) m += varD
      }
      reduceVariablesHelper(xs, m)
    }
    case Nil => map
  }
}

object ArgsHelper {
  def mangle(list: TParams): String = {
    val b = new StringBuilder

    for (a <- list) b.append(s"${a.name}_")

    b.take(b.length - 2)
    b.mkString
  }

  def codeGen(args: TArgs)(implicit context: CodeGenContext): String = {
    val b = new StringBuilder

    for (arg <- args) {
      b.append(arg.codegen(0, context) ++ ",")
    }

    b.take(b.length - 2).mkString
  }
}
