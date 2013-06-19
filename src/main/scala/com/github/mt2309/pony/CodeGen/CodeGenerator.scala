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

    val headerBuilder = new StringBuilder(CodeGenerator.headerString)
    val sourceBuilder = new StringBuilder(CodeGenerator.sourceString)

    sourceBuilder.append(s"\n\nvoid initialise(void)\n{\n\tclazz_set_size = $longArraySize;\n")

    // Generate the header
    for (clazz <- classes) {
      headerBuilder.append(s"unsigned long * ${clazz._1}_id;\n")

      sourceBuilder.append(s"\t${clazz._1}_id = initialise_bit_set(${clazz._3});\n")


      for (typebody <- clazz._2.methods) {
        headerBuilder.append(s"pony_clazz** ${clazz._1}_${typebody._2.mangle}(pony_clazz* this, pony_clazz** args);\n")
      }
    }

    sourceBuilder.append("}\n\n")

    for (clazz <- classes) {
      clazz._2 match {
        case conc: ConcreteClass => {
          headerBuilder.append(s"pony_clazz * ${clazz._1}_construct();\n")
          sourceBuilder.append(conc.defaultConstructor)

          for (body <- conc.methods) {
            headerBuilder.append(s"pony_clazz** ${clazz._1}_${body._2.mangle}(pony_clazz* this, pony_clazz** args);\n")
            sourceBuilder.append(s"pony_clazz**\n${clazz._1}_${body._2.mangle}(pony_clazz* this, pony_clazz** args)\n")
            sourceBuilder.append(body._2.codeGen)
          }
        }
        case _ =>
      }
    }
    println(headerBuilder.mkString)
    println("\n\n\n\n\n")
    println(sourceBuilder.mkString)

  }

}

object CodeGenerator {

  val headerString = {
    "#include <stdlib.h>\n#include <stdio.h>\n#include <stdbool.h>\n\n" ++
      "#include \"pony_class.h\"\n\n#ifndef PONY_PROGRAM_H\n#define PONY_PROGRAM_H\n\nvoid initialise(void);\nunsigned int clazz_set_size;\n\n"
  }

  val sourceString = "#include \"pony_class_ids.h\"\n\n"
}
