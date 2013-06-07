package com.github.mt2309.pony.CodeGen

import com.github.mt2309.pony.Typer.{TModuleMember, TypedModule}
import com.github.mt2309.pony.Common.{TypeId, FileContents}

import org.jllvm.{LLVMConstant, LLVMInstructionBuilder, LLVMModule, LLVMContext}
import com.github.mt2309.pony.CompilationUnit.CompilationUnit

/**
 * User: mthorpe
 * Date: 29/05/2013
 * Time: 16:43
 */
final class CodeGenerator(val units: IndexedSeq[CompilationUnit], val output: String) {

  val modules: IndexedSeq[TypedModule] = units.map(_.typeIt).flatten

  def codeGen(): Unit = {
    val moduleCount = modules.map(_.classes.size).sum

    val classes: IndexedSeq[(TypeId, TModuleMember)] = modules.map(_.classes).flatten


  }

}
