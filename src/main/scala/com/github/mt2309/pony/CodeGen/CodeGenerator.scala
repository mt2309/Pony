package com.github.mt2309.pony.CodeGen

import com.github.mt2309.pony.Common.TypeId

import com.github.mt2309.pony.CompilationUnit.CompilationUnit

import com.github.mt2309.pony.Typer._

/**
 * User: mthorpe
 * Date: 29/05/2013
 * Time: 16:43
 */
final class CodeGenerator(val units: IndexedSeq[CompilationUnit], val output: String) {

  val modules: IndexedSeq[TypedModule] = units.map(_.typeIt).flatten

  def codeGen(): Unit = {
    val classes: IndexedSeq[(TypeId, TModuleMember, Int)] = (modules.map(_.classes).flatten ++ tPrimitiveTypes.map(t => t.typename -> t)).zipWithIndex.map(t => (t._1._1, t._1._2, t._2))

    val longArraySize = classes.size / 64 + 1

    for (clazz <- classes) {
      println(s"unsigned long * ${clazz._1}_id")
    }

    println(s"\n\nvoid initialise()\n{\n\tclazz_set_size = $longArraySize;")

    for (clazz <- classes) {
      println(s"\t${clazz._1}_id = initialise_bit_set(${clazz._3});")
    }

    println("}")


  }

}
