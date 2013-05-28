package com.github.mt2309.pony.Parser

import scala.util.parsing.input.OffsetPosition

/**
 * User: mthorpe
 * Date: 28/05/2013
 * Time: 23:06
 */
object Tokens {

  trait Token[A] {
    def value: A
    def pos: OffsetPosition
    def length: Int
  }
  object Token {
    def apply[A](tokValue: A, tokPos: OffsetPosition, tokLength: Int) =
      new Token[A] { val value = tokValue; val pos = tokPos; val length = tokLength }

    def unapply[A](t: Token[A]): Option[(A, OffsetPosition, Int)] =
      Some(t.value, t.pos, t.length)
  }
  case class IdentToken(value: String, pos: OffsetPosition, length: Int) extends Token[String]
  case class IntToken(value: Int, pos: OffsetPosition, length: Int) extends Token[Int]
  case class KeywordToken(value: String, pos: OffsetPosition, length: Int) extends Token[String]
  case class WhiteSpaceToken(value: String, pos: OffsetPosition, length: Int) extends Token[String]
}
