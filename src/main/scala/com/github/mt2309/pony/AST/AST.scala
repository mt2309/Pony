package com.github.mt2309.pony.AST

/**
 * User: mthorpe
 * Date: 26/04/2013
 * Time: 00:16
 */
sealed abstract class Token(filename: String, line: Int, linePos: Int, var token: TokenType)

case class StringToken(string:String, tokType: TokenType, f:String, l: Int, lp: Int) extends Token(f, l, lp, tokType)
case class DoubleToken(double:Double, tokType: TokenType, f:String, l: Int, lp: Int) extends Token(f, l, lp, tokType)
case class IntToken(int:Int, tokType: TokenType, f:String, l: Int, lp: Int) extends Token(f, l, lp, tokType)
case class ProgramToken(tokType: TokenType, f:String, l: Int, lp: Int) extends Token(f, l, lp, tokType)

final case class AST(token: Token, var sibling: AST, children:Seq[AST] = AST.emptyChildren)

object AST {

  val AST_CHILDREN = 7

  def emptyChildren: Seq[AST] = Array.fill(7)(null).toVector
}