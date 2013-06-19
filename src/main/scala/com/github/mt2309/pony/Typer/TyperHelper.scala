package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.Common._
import annotation.tailrec

/**
 * User: mthorpe
 * Date: 11/06/2013
 * Time: 03:38
 */
object TyperHelper {

  def typeToClass(opt: Option[TOfType]): String = opt match {
    case Some(x) => {
      if (x.isPrimitive) {
        x.maximalType.cTypeName
      }
      else {
        "pony_clazz * "
      }
    }
    case None => {
      "pony_clazz * "
    }
  }


  def typeToConstructor(opt: Option[TOfType]): String = opt match {
    case Some(x) => {
      if (x.isPrimitive) {
        x.maximalType.defaultConstructor
      }
      else if (x.isEmpty) {
        "NULL"
      }
      else {
        x.defaultConstructor
      }
    }
    case None => ???
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

  def codeGen(args: TArgs): String = {
    val b = new StringBuilder

    for (arg <- args) {
      b.append(arg.codeGen ++ ",")
    }


    b.take(b.length - 2).mkString
  }
}
