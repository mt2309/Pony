package com.github.mt2309.pony.CodeGen

import com.github.mt2309.pony.Typer.{TModuleMember, TypedModule}
import com.github.mt2309.pony.Common.{TypeId, FileContents}

import org.jllvm.{LLVMConstant, LLVMInstructionBuilder, LLVMModule, LLVMContext}

/**
 * User: mthorpe
 * Date: 29/05/2013
 * Time: 16:43
 */
final class CodeGenerator(val modules: Set[TypedModule]) {

  def generateSet: Set[CompleteFile] = modules.map(generate).flatten
  def generate(m: TypedModule): Set[CompleteFile] = m.classes.map(t => new ModuleCodeGenerator(t._1, t._2).generate).toSet
}

final class ModuleCodeGenerator(typeId: TypeId, moduleMember: TModuleMember) {
  val const = LLVMConstant


  def generate: CompleteFile = {

    val header = new Header(typeId, createHeader)

    new CompleteFile(header, new SourceFile(header, createSourceFile))
  }

  def createHeader: FileContents = {

    ???
  }

  def createSourceFile: FileContents = ???
}
