package com.github.mt2309.pony.CodeGen

import com.github.mt2309.pony.Common.{TypeId,ID,TabbedBuilder}
import com.github.mt2309.pony.CompilationUnit.CompilationUnit
import com.github.mt2309.pony.Typer._

import java.io.PrintWriter

/**
 * User: mthorpe
 * Date: 29/05/2013
 * Time: 16:43
 */

final case class CodeGenContext(currentClazz: ConcreteClass, workingID: ID, functionName: Option[ID]) {
  def name: TypeId = currentClazz.name
  def variables: Map[ID, Option[TOfType]] = currentClazz.variables
}

final class CodeGenerator(val units: IndexedSeq[CompilationUnit], val output: String) {

  private val modules: IndexedSeq[TypedModule] = units.map(_.typeIt).flatten

  def codeGen(): Unit = {
    val classes: IndexedSeq[(TypeId, TModuleMember, Int)] = (modules.map(_.classes).flatten ++ tPrimitiveTypes.map(t => t.name -> t)).zipWithIndex.map(t => (t._1._1, t._1._2, t._2))

    val intArraySize = classes.size / 64 + 1

    val headerBuilder = new StringBuilder(CodeGenerator.headerString)
    val initBuilder = new StringBuilder(CodeGenerator.sourceString)

    initBuilder.append(s"\n\nvoid initialise(int argc, char *argv[])\n{\n\tclazz_set_size = $intArraySize;\n")

    // Generate the header
    for (clazz <- classes) {
      headerBuilder.append(s"static unsigned int * ${clazz._1}_id;\n")
      initBuilder.appendln(s"${clazz._1}_id = initialise_bit_set(${clazz._3});")(1)

      if (clazz._2.isInstanceOf[ConcreteClass]) {
        initBuilder.appendln(s"static_${clazz._1} = create_static_${clazz._1}();")(1)
      }
      else {
        initBuilder.appendln("")(1)
      }
    }

    headerBuilder.append("\n")

    val sourceBuilder = new StringBuilder

    for (clazz <- classes) {
      clazz._2 match {
        case conc: ConcreteClass => {
          val context = new CodeGenContext(conc, "this", None)
          headerBuilder.appendln(s"static pony_clazz * ${clazz._1}_init(void);")(0)
          headerBuilder.appendln(s"static static_clazz * static_${clazz._1} = NULL;")(0)
          headerBuilder.appendln(s"static static_clazz * create_static_${clazz._1}(void);")(0)
          sourceBuilder.append(conc.initialiseStatic(1))
          sourceBuilder.append(conc.codegen(1, context))

          conc match {
            case actor: TActor =>
              headerBuilder.append(s"static void ${conc.name}_dispatch(actor_t*, void*, type_t*, uint64_t, arg_t);\n")
              sourceBuilder.append(actor.createDispatch(1, context))
            case _ =>
          }

          for (body <- conc.methods) {
            headerBuilder.appendln(body._2.header(conc))(0)
            if (body._1 == "main") {
              initBuilder.appendln(s"pony_start(argc, argv, pony_create(${conc.name}_dispatch, NULL));")(1)
//              initBuilder.appendln(s"${conc.name}_${body._1}(NULL, create_args(2, create_int_var(atoi(argv[1])), create_int_var(atoi(argv[2]))));")(1)
            }
            sourceBuilder.append(body._2.codegen(1, context.copy(functionName = Some(body._1))))
          }
        }
        case _ =>
      }
    }

    headerBuilder.append("\n#endif")
    initBuilder.append("}\n\n")

    val header = new PrintWriter(output ++ "/pony_class_ids.h", "UTF-8")
    val source = new PrintWriter(output ++ "/pony_class_ids.c", "UTF-8")

    header.println(headerBuilder.mkString)
    source.println(initBuilder.mkString)
    source.println(sourceBuilder.mkString)

    header.close()
    source.close()
  }

}

private object CodeGenerator {

  val headerString: String = {
    val b = new StringBuilder
    b.append("#include <stdlib.h>\n#include <stdio.h>\n#include <stdbool.h>\n\n")
    b.append("#include \"pony_class.h\"\n\n#ifndef PONY_PROGRAM_H\n#define PONY_PROGRAM_H\n\nvoid initialise(int argc, char *argv[]);\nunsigned int clazz_set_size;\n\n")

    b.mkString
  }

  val sourceString: String = "#include \"pony_class_ids.h\"\n\n"
}
