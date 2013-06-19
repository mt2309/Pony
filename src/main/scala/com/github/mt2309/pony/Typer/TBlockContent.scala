package com.github.mt2309.pony.Typer

import com.github.mt2309.pony.Common._

/**
 * User: mthorpe
 * Date: 19/06/2013
 * Time: 23:07
 */
trait TBlockContent extends Typer {
  def codeGen: String
}

final class TReturn(implicit val scope: Scope) extends TBlockContent {
  override def codeGen = "goto return_label;\n"
}

final class TThrow(implicit val scope: Scope) extends TBlockContent {
  override def codeGen = "goto cleanup;\n"
}

final class TBreak(implicit val scope: Scope) extends TBlockContent {
  override def codeGen = "break;\n"
}

final class TContinue(implicit val scope: Scope) extends TBlockContent {
  override def codeGen = "continue;\n"
}

final case class TVarDec(id: ID, ofType: Option[TOfType], expr: Option[TExpr])(implicit val scope: Scope) extends TBlockContent {
  override def codeGen = {
    val b = new StringBuilder

    b.append(s"pony_clazz ** $id ")

    if (expr.isDefined)
      b.append(s"= ${expr.get.codeGen};\n")
    else
      b.append(s"= ${ofType.get.defaultConstructor};\n")

    b.mkString
  }

  def constructor: String = ofType.get.defaultConstructor
}

final case class TMatch(list: List[(TExpr,TCaseBlock)])(implicit val scope: Scope) extends TBlockContent {
  override def codeGen = {
    val b = new StringBuilder


    b.mkString
  }
}

final case class TDoLoop(block: TBlock, whileExpr: TExpr)(implicit val scope: Scope) extends TBlockContent {
  override def codeGen = s"do (${whileExpr.codeGen})\n${block.codeGen}\n"
}

final case class TWhileLoop(whileExpr: TExpr, block: TBlock)(implicit val scope: Scope) extends TBlockContent {
  override def codeGen = {
    val b = new StringBuilder
    b.append(s"while (${whileExpr.codeGen})\n")
    b.append(block.codeGen)

    b.append("\n")

    b.mkString
  }
}

final case class TForLoop(forVars: List[TForVar], inExpr: TExpr, block: TBlock)(implicit val scope: Scope) extends TBlockContent {
  override def codeGen = ???
}

final case class TConditional(conditionalList: List[(TExpr, TBlock)], elseBlock: Option[TBlock])(implicit val scope: Scope) extends TBlockContent {
  override def codeGen = {
    val builder = new StringBuilder
    builder.append(s"if (${conditionalList.head._1.codeGen})\n${conditionalList.head._2.codeGen}\n")

    for (cond <- conditionalList.tail)
      builder.append(s"else if (${cond._1})\n${cond._2}\n")

    for (e <- elseBlock)
      builder.append(s"else ${e.codeGen}")

    builder.mkString
  }
}

final case class TAssignment(lValues: List[TLValue], expr: Option[TExpr])(implicit val scope: Scope) extends TBlockContent {
  override def codeGen = {
    val b = new StringBuilder

    b.mkString
  }
}
