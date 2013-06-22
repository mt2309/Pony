package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.Common._
import annotation.tailrec
import com.github.mt2309.pony.CodeGen.CodeGenContext

/**
 * User: mthorpe
 * Date: 11/06/2013
 * Time: 03:38
 */
object TyperHelper {

  def extractFrom(opt: Option[TOfType]): String = "->" ++ structName(opt)

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
