package com.github.mt2309.pony.Parser

import scala.util.parsing.combinator.RegexParsers

import scala.language.postfixOps

object PonyParser extends RegexParsers {

  def parse(file: String) {
    parse(module, file) match {
      case Success(_, _) => println("success")
      case e: NoSuccess => println("failure\t" + e.msg)
    }
  }

  def module = (use | declare | typeParser | traitParser | objectParser).*

  def use = "use" ~ ((typeId ~ "=")?) ~ string
  def declare = "declare" ~ typeclass ~ is ~ declaremap
  def declaremap = "{" ~ ((map ~ ",")*) ~ map ~ "}"
  def map = (id ~ "=" ~ id)
  def typeParser = "type" ~ typeId ~ ofType ~ is
  def actor = "actor" ~ typeId ~ formalargs ~ is ~ typeBody
  def traitParser = "trait" ~ typeId ~ formalargs ~ is ~ typeBody
  def objectParser = "object" ~ typeId ~ formalargs ~ is ~ typeBody
  def is = ("is" ~ ((typeclass ~ ",") ?) ~ typeclass).?
  def typeBody = "{" ~ ((field | delegate | constructor | ambient | function | message)+) ~ "}"

  def message = "message" ~ mode ~ id ~ formalargs ~ args ~ (";" | block)
  def function = "function" ~ mode ~ id ~ formalargs ~ args ~ (("->" ~ args)?) ~ ("throws"?) ~ (";" | block)

  def field = "var" ~ id ~ ofType ~ (";" | ("=" ~ expr))
  def delegate = "delegate" ~ id ~ ofType
  def constructor = "new" ~ mode ~ id ~ formalargs ~ args ~ ("throws"?) ~ (";" | block)
  def ambient = "ambient" ~ mode ~ id ~ formalargs ~ args ~ ("throws"?) ~ (";" | block)

  def ofType: Parser[Any] = (":" ~ ((typeElement ~ "|") *) ~ typeElement).?

  def typeElement = (partialType | typeclass | lambda)

  def partialType: Parser[Any] = "\\" ~ typeclass
  def typeclass: Parser[Any] = typeId ~ (("::" ~ typeId)?) ~ mode ~ formalargs

  def lambda = "lambda" ~ mode ~ args ~ (("->" ~ args)?) ~ "throws" ~ (("is" ~ block)?)

  def formalargs = ("(" ~ parserList(arg, ",") ~ ")").?

  def block: Parser[Any] = { "{" ~
    ((block | conditional | forLoop | whileLoop | doLoop | matchParser |
      "return" | "break" | "continue" | "throw" | assignment )*) ~ (catchBlock?) ~ (always?) ~ "}"
  }

  def assignment = ((lvalue ~ ",")*) ~ lvalue ~ (("=" ~ expr)?)

  def lvalue = ("var" ~ id ~ ofType) | command

  def catchBlock = "catch" ~ block

  def always = "always" ~ block

  def matchParser = "match" ~ parserList(expr, ",") ~ "{" ~ (caseBlock+) ~ "}"

  def caseBlock = "case" ~ ((("if" ~ expr) | parserList(casevar, ","))?) ~ block

  def casevar = (expr?) ~ ("as" ~ forVar)

  def doLoop = "do" ~ block ~ "while" ~ expr
  def whileLoop = "while" ~ expr ~ block
  def forLoop = "for" ~ parserList(forVar, ",") ~ "in" ~ expr ~ block

  def forVar = id ~ ofType

  def conditional: Parser[Any] = "if" ~ expr ~ block ~ (("else" ~ (conditional | block))?)

  def args = "(" ~ (parserList(arg, ",")?) ~ ")"
  def arg = expr ~ ofType ~ (("=" ~ expr)?)

  def expr = unary ~ ((("+" | "-" | "*" | "/" | "%" | "<<" | ">>" | ">" | "<" | ">=" | "<=" | "!=" | "-" | "#=" | "~=" | "|" | "^" | "&" ) ~ unary)?)

  def unary: Parser[Any] = (("\\" | "-" | "!") ~ unary) | ("lambda" | lambda) | command

  def command = (("(" ~ expr ~ ")") | ("[" ~ parserList(arg, ",") ~ "]") | atom) ~ (args | ("." ~ id ~ formalargs ~ args))

  def atom = "this" | "true" | "false" | int | string | id | typeId

  def mode = "!" | "@" | "~" | ("[:" ~ expr ~ "]")


  def typeId: Parser[String] = """[A-Z][a-zA-Z_]+""".r
  def string: Parser[String] = ("\""+"""([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*"""+"\"").r
  def int: Parser[Int] = """-?\d+""".r ^^ { _.toInt }
  def float: Parser[Double] = """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r ^^ {_.toDouble}

  def id: Parser[String] = """[a-z][a-zA-Z_]+""".r

  def parserList(p: Parser[Any], tok: String) = ((p ~ tok)*) ~ p

}