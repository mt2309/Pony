package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.Common._

/**
 * User: mthorpe
 * Date: 19/06/2013
 * Time: 23:07
 */
trait TBlockContent extends Typer {
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String
}

final class TReturn(implicit val scope: Scope) extends TBlockContent {
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = {
    " "*2*indent ++ "goto return_label;\n"
  }
}

final class TThrow(implicit val scope: Scope) extends TBlockContent {
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = " "*2*indent ++ "goto cleanup;\n"
}

final class TBreak(implicit val scope: Scope) extends TBlockContent {
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = " "*2*indent ++ "break;\n"
}

final class TContinue(implicit val scope: Scope) extends TBlockContent {
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = " "*2*indent ++ "continue;\n"
}

final case class TVarDec(id: ID, ofType: Option[TOfType], expr: Option[TExpr])(implicit val scope: Scope) extends TBlockContent {
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = {
    val b = new StringBuilder

    b.append(" "*2*indent ++ s"pony_clazz ** $id ")

    expr match {
      case Some(x) => b.append(s"= ${x.codegen};\n")
      case None =>    b.append(s"= ${ofType.get.codegen};\n")
    }

    b.mkString
  }

  def constructor(implicit indent: Int, currentClazz: ConcreteClass): String = ofType.get.codegen
}

final case class TMatch(list: List[(TExpr,TCaseBlock)])(implicit val scope: Scope) extends TBlockContent {
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = ???
}

final case class TDoLoop(block: TBlock, whileExpr: TExpr)(implicit val scope: Scope) extends TBlockContent {
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = {
    val b = new StringBuilder

    b.appendln(s"do (${whileExpr.codegen}")

    b.appendln(block.codegen)(indent + 1)

    b.mkString
  }
}

final case class TWhileLoop(whileExpr: TExpr, block: TBlock)(implicit val scope: Scope) extends TBlockContent {
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = {
    val b = new StringBuilder
    b.append(s"while (${whileExpr.codegen})\n")
    b.append(block.codegen)

    b.append("\n")

    b.mkString
  }
}

final case class TForLoop(forVars: List[TForVar], inExpr: TExpr, block: TBlock)(implicit val scope: Scope) extends TBlockContent {
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = ???
}

final case class TConditional(conditionalList: List[(TExpr, TBlock)], elseBlock: Option[TBlock])(implicit val scope: Scope) extends TBlockContent {
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = {
    val builder = new StringBuilder
    builder.appendln(s"if (${conditionalList.head._1.codegen})\n${conditionalList.head._2.codegen(indent + 1, currentClazz)}")

    for (cond <- conditionalList.tail)
      builder.appendln(s"else if (${cond._1.codegen})\n${cond._2.codegen(indent + 1, currentClazz)}")

    for (e <- elseBlock)
      builder.append(s"else ${e.codegen(indent + 1, currentClazz)}")

    builder.mkString
  }
}

final case class TAssignment(lValues: List[TLValue], expr: Option[TExpr])(implicit val scope: Scope) extends TBlockContent {
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = {
    val b = new StringBuilder

    for (e <- expr) {

      b.appendln(s"${TyperHelper.typeToClass(e.extractOfType)} result_${e.hashCode.abs} = ${e.codegen};")

      if (TyperHelper.isPrimitive(e.extractOfType) || TyperHelper.isSingleRes(e.extractOfType)) {
        b.appendln(s"${lValues.head.codegen} = result_${e.hashCode.abs};")
      }
      else {
        for (lVal <- lValues.zipWithIndex) {
          b.appendln(s"${lVal._1.codegen} = result_${e.hashCode.abs}[${lVal._2}]->${TyperHelper.structName(lVal._1.ofType)};")
        }
        b.append("")
      }
    }

    b.mkString
  }
}

final case class TBlock(contents: List[TBlockContent] = List.empty, catchBlock: Option[TBlock] = None, alwaysBlock: Option[TBlock] = None)
                       (implicit val scope: Scope) extends Typer with TBlockContent {
  override def codegen(implicit indent: Int, currentClazz: ConcreteClass): String = {
    val b = new StringBuilder

    b.appendln("{")(indent - 1)

    for (c <- contents) {
      b.append(c.codegen)
    }

    b.appendln("}")(indent - 1)


    b.mkString
  }
}
