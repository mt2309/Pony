package com.github.mt2309.pony.CodeGen

import com.github.mt2309.pony.Common.{TypeId,TabbedBuilder}
import com.github.mt2309.pony.CompilationUnit.CompilationUnit
import com.github.mt2309.pony.Typer._
import java.io.{PrintWriter, File}

/**
 * User: mthorpe
 * Date: 29/05/2013
 * Time: 16:43
 */
final class CodeGenerator(val units: IndexedSeq[CompilationUnit], val output: String) {

  val modules: IndexedSeq[TypedModule] = units.map(_.typeIt).flatten

  def codeGen(): Unit = {
    val classes: IndexedSeq[(TypeId, TModuleMember, Int)] = (modules.map(_.classes).flatten ++ tPrimitiveTypes.map(t => t.name -> t)).zipWithIndex.map(t => (t._1._1, t._1._2, t._2))

    val intArraySize = classes.size / 64 + 1

    val headerBuilder = new StringBuilder(CodeGenerator.headerString)
    val sourceBuilder = new StringBuilder(CodeGenerator.sourceString)

    sourceBuilder.append(s"\n\nvoid initialise(void)\n{\n  clazz_set_size = $intArraySize;\n")

    // Generate the header
    for (clazz <- classes) {
      headerBuilder.append(s"unsigned int * ${clazz._1}_id;\n")
      sourceBuilder.appendln(s"${clazz._1}_id = initialise_bit_set(${clazz._3});")(1)

      if (clazz._2.isInstanceOf[ConcreteClass]) {
        sourceBuilder.appendln(s"static_${clazz._1} = create_static_${clazz._1}();")(1)
      }
      else {
        sourceBuilder.appendln("")(1)
      }
    }

    headerBuilder.append("\n")
    sourceBuilder.append("}\n\n")

    for (clazz <- classes) {
      clazz._2 match {
        case conc: ConcreteClass => {
          headerBuilder.appendln(s"pony_clazz * ${clazz._1}_construct(void);")(0)
          headerBuilder.appendln(s"static_clazz * static_${clazz._1} = NULL;")(0)
          headerBuilder.appendln(s"static_clazz * create_static_${clazz._1}(void);")(0)
          sourceBuilder.append(conc.initialiseStatic(1))
          sourceBuilder.append(conc.codegen(1, conc))

          for (body <- conc.methods) {
            headerBuilder.appendln(body._2.header(conc))(0)
            sourceBuilder.append(body._2.codegen(1, conc))
          }
        }
        case _ =>
      }
    }

    headerBuilder.append("#endif")

    val header = new PrintWriter(output ++ "/pony_class_ids.h", "UTF-8")
    val source = new PrintWriter(output ++ "/pony_class_ids.c", "UTF-8")

    header.println(headerBuilder.mkString)
    source.println(sourceBuilder.mkString)

    header.close()
    source.close()
  }

}

object CodeGenerator {

  val headerString = {
    "#include <stdlib.h>\n#include <stdio.h>\n#include <stdbool.h>\n\n" ++
      "#include \"pony_class.h\"\n\n#ifndef PONY_PROGRAM_H\n#define PONY_PROGRAM_H\n\nvoid initialise(void);\nunsigned int clazz_set_size;\n\n"
  }

  val sourceString = "#include \"pony_class_ids.h\"\n\n"
}
