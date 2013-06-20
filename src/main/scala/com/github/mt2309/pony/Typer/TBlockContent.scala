package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.Common._

/**
 * User: mthorpe
 * Date: 19/06/2013
 * Time: 23:07
 */
trait TBlockContent extends Typer {
  def codegen(implicit indent: Int): String
}

final class TReturn(implicit val scope: Scope) extends TBlockContent {
  override def codegen(implicit indent: Int) = " "*2*indent ++ "goto return_label;\n"
}

final class TThrow(implicit val scope: Scope) extends TBlockContent {
  override def codegen(implicit indent: Int) = " "*2*indent ++ "goto cleanup;\n"
}

final class TBreak(implicit val scope: Scope) extends TBlockContent {
  override def codegen(implicit indent: Int) = " "*2*indent ++ "break;\n"
}

final class TContinue(implicit val scope: Scope) extends TBlockContent {
  override def codegen(implicit indent: Int) = " "*2*indent ++ "continue;\n"
}

final case class TVarDec(id: ID, ofType: Option[TOfType], expr: Option[TExpr])(implicit val scope: Scope) extends TBlockContent {
  override def codegen(implicit indent: Int) = {
    val b = new StringBuilder

    b.append(" "*2*indent ++ s"pony_clazz ** $id ")

    expr match {
      case Some(x) => b.append(s"= ${x.codegen};\n")
      case None =>    b.append(s"= ${ofType.get.codegen};\n")
    }

    b.mkString
  }

  def constructor(implicit indent: Int): String = ofType.get.codegen
}

final case class TMatch(list: List[(TExpr,TCaseBlock)])(implicit val scope: Scope) extends TBlockContent {
  override def codegen(implicit indent: Int) = ???
}

final case class TDoLoop(block: TBlock, whileExpr: TExpr)(implicit val scope: Scope) extends TBlockContent {
  override def codegen(implicit indent: Int) = {
    val b = new StringBuilder

    b.appendln(s"do (${whileExpr.codegen}")

    b.appendln(block.codegen)(indent + 1)

    b.mkString
  }
}

final case class TWhileLoop(whileExpr: TExpr, block: TBlock)(implicit val scope: Scope) extends TBlockContent {
  override def codegen(implicit indent: Int) = {
    val b = new StringBuilder
    b.append(s"while (${whileExpr.codegen})\n")
    b.append(block.codegen)

    b.append("\n")

    b.mkString
  }
}

final case class TForLoop(forVars: List[TForVar], inExpr: TExpr, block: TBlock)(implicit val scope: Scope) extends TBlockContent {
  override def codegen(implicit indent: Int) = ???
}

final case class TConditional(conditionalList: List[(TExpr, TBlock)], elseBlock: Option[TBlock])(implicit val scope: Scope) extends TBlockContent {
  override def codegen(implicit indent: Int) = {
    val builder = new StringBuilder
    builder.append(s"if (${conditionalList.head._1.codegen})\n${conditionalList.head._2.codegen}\n")

    for (cond <- conditionalList.tail)
      builder.append(s"else if (${cond._1})\n${cond._2}\n")

    for (e <- elseBlock)
      builder.append(s"else ${e.codegen}")

    builder.mkString
  }
}

final case class TAssignment(lValues: List[TLValue], expr: Option[TExpr])(implicit val scope: Scope) extends TBlockContent {
  override def codegen(implicit indent: Int) = {
    val b = new StringBuilder

    b.mkString
  }
}

final case class TBlock(contents: List[TBlockContent], catchBlock: Option[TBlock], alwaysBlock: Option[TBlock])(implicit val scope: Scope) extends Typer with TBlockContent {
  override def codegen(implicit indent: Int) = {
    val b = new StringBuilder

    b.appendln("{")

    for (c <- contents) {
      b.append(c.codegen)
    }

    b.appendln("}")


    b.mkString
  }
}
