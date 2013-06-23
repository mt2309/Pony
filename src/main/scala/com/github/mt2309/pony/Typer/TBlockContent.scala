package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.Common._
import com.github.mt2309.pony.CodeGen.CodeGenContext

/**
 * User: mthorpe
 * Date: 19/06/2013
 * Time: 23:07
 */
trait TBlockContent extends Typer {
  override def codegen(implicit indent: Int, context: CodeGenContext): String
}

final class TReturn(implicit val scope: Scope) extends TBlockContent {
  override def codegen(implicit indent: Int, context: CodeGenContext): String = {
    " "*2*indent ++ "goto return_label;\n"
  }

  override def toString = "TReturn"
}

// Call out to a native (C) function
// For now we're just going to use it to print stuff
final class TNative(implicit val scope: Scope) extends TBlockContent {
  override def codegen(implicit indent: Int, context: CodeGenContext): String = {
    val b = new StringBuilder

    b.appendln("printf(\"%f\\n\", x);")

    b.mkString
  }

  override def toString = "TNative"
}

final class TThrow(implicit val scope: Scope) extends TBlockContent {
  override def codegen(implicit indent: Int, context: CodeGenContext): String = " "*2*indent ++ "goto cleanup;\n"

  override def toString = "TThrow"
}

final class TBreak(implicit val scope: Scope) extends TBlockContent {
  override def codegen(implicit indent: Int, context: CodeGenContext): String = " "*2*indent ++ "break;\n"

  override def toString = "TBreak"
}

final class TContinue(implicit val scope: Scope) extends TBlockContent {
  override def codegen(implicit indent: Int, context: CodeGenContext): String = " "*2*indent ++ "continue;\n"

  override def toString = "TContinue"
}

final case class TVarDec(id: ID, ofType: Option[TOfType], expr: Option[TExpr])(implicit val scope: Scope) extends TBlockContent {
  override def codegen(implicit indent: Int, context: CodeGenContext): String = {
    val b = new StringBuilder

    val e = expr.get

    b.appendTo(s"${TyperHelper.typeToClass(ofType)} $id = ${e.codegen}")

    b.mkString
  }

  def constructor(implicit indent: Int, context: CodeGenContext): String = TyperHelper.typeToConstructor(ofType)

  override def toString = s"TVarDec(id = $id, ofType = $ofType, expr = $expr"
}

final case class TMatch(list: List[(TExpr,TCaseBlock)])(implicit val scope: Scope) extends TBlockContent {
  override def codegen(implicit indent: Int, context: CodeGenContext): String = ???
}

final case class TDoLoop(block: TBlock, whileExpr: TExpr)(implicit val scope: Scope) extends TBlockContent {
  override def codegen(implicit indent: Int, context: CodeGenContext): String = {
    val b = new StringBuilder

    b.appendln(s"do (${whileExpr.codegen}")

    b.appendln(block.codegen)(indent + 1)

    b.mkString
  }

  override def toString = s"DoLoop(doExpr = $whileExpr, block = $block)"
}

final case class TWhileLoop(whileExpr: TExpr, block: TBlock)(implicit val scope: Scope) extends TBlockContent {
  override def codegen(implicit indent: Int, context: CodeGenContext): String = {
    val b = new StringBuilder
    b.appendln(s"while (${whileExpr.codegen})")
    b.append(block.codegen(indent + 1, context))

    b.mkString
  }

  override def toString = s"WhileLoop(whileExpr = $whileExpr, block = $block)"
}

final case class TForLoop(forVars: List[TForVar], range: (TExpr, TExpr), block: TBlock)(implicit val scope: Scope) extends TBlockContent {
  override def codegen(implicit indent: Int, context: CodeGenContext): String = {
    val b = new StringBuilder

    val head = forVars.head

    b.appendln(s"for (${TyperHelper.typeToClass(head.ofType)} ${head.id} = ${range._1.codegen}; ${head.id} < ${range._2.codegen}; ${head.id}++)")
    b.append(block.codegen(indent + 1, context))

    b.mkString
  }

  override def toString = s"TForLoop(forVars = $forVars, in = $range, block = $block)"
}

final case class TConditional(conditionalList: List[(TExpr, TBlock)], elseBlock: Option[TBlock])(implicit val scope: Scope) extends TBlockContent {
  override def codegen(implicit indent: Int, context: CodeGenContext): String = {

    val builder = new StringBuilder
    builder.appendln(s"if (${conditionalList.head._1.codegen})\n${conditionalList.head._2.codegen(indent + 1, context)}")

    for (cond <- conditionalList.tail)
      builder.appendln(s"else if (${cond._1.codegen})\n${cond._2.codegen(indent + 1, context)}")

    for (e <- elseBlock)
      builder.append(s"else ${e.codegen(indent + 1, context)}")

    builder.mkString
  }

  override def toString = s"TConditional(conditionals = $conditionalList, elseBlock = $elseBlock)"
}

final case class TAssignment(lValues: List[TLValue], expr: Option[TExpr])(implicit val scope: Scope) extends TBlockContent {
  override def codegen(implicit indent: Int, context: CodeGenContext): String = {
    val b = new StringBuilder

    if (expr.isDefined) {

      val e = expr.get

      b.appendln(s"${TyperHelper.typeToClass(e.ofType)} result_${e.hashCode.abs} = ${e.codegen};")

      if (TyperHelper.isPrimitive(e.ofType) || TyperHelper.isSingleRes(e.ofType)) {
        b.appendln(s"${lValues.head.codegen} = result_${e.hashCode.abs};")
      }
      else {
        for (lVal <- lValues.zipWithIndex) {
          b.appendln(s"${lVal._1.codegen} = result_${e.hashCode.abs}[${lVal._2}]->${TyperHelper.structName(lVal._1.ofType)};")
        }
        b.append("")
      }
    }
    else {
      for (lVal <- lValues) {
        b.append(lVal.codegen ++ ";\n")
      }
    }

    b.mkString
  }

  override def toString = s"TAssignment(lvalues = $lValues, expr = $expr"
}

final case class TBlock(contents: List[TBlockContent] = List.empty, catchBlock: Option[TBlock] = None, alwaysBlock: Option[TBlock] = None)
                       (implicit val scope: Scope) extends Typer with TBlockContent {
  override def codegen(implicit indent: Int, context: CodeGenContext): String = {
    val b = new StringBuilder

    b.appendln("{")(indent - 1)

    for (c <- contents) {
      b.append(c.codegen)
    }

    b.appendln("}")(indent - 1)

    b.mkString
  }

  override def toString = s"TBlock(contents = $contents, catch = $catchBlock, always = $alwaysBlock)"
}
