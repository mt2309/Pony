package com.github.mt2309.pony.Parser

import com.github.mt2309.pony.AST._
import com.github.mt2309.pony.AST.AST
import util.parsing.combinator.RegexParsers

import language.postfixOps

final class PonyParser(val filename: String, val file: String) {


  type F = () => AST

  final case class Alt(id: TokenType, f: F)

  type AltList = Seq[Alt]

  private val SLOT_MAX = Int.MaxValue

  val errorList:ErrorList = new ErrorList
  val lexer: Lexer = new Lexer(filename, file, errorList)

  val t = lexer.next
  val m_ast: AST = module

  if (errorList.size > 0) {
    println("Parse errors detected")
    errorList.foreach(println(_))
  }

  def module: AST = {
    val alt = Seq(
      new Alt(TK_USE, () => use),
      new Alt(TK_DECLARE, () => declare),
      new Alt(TK_TYPE, () => defType),
      new Alt(TK_OBJECT, () => defObject),
      new Alt(TK_ACTOR, () => actor)
    )

    val ast = newAST(TK_MODULE)

    ruleAltList(alt, ast, 0)

    expect(TK_EOF, ast, Int.MaxValue)
  }

  def use: AST = {
    val ast = expectAST(TK_USE)

    if (accept(TK_TYPEID, ast, 0)) {
      expect(TK_ASSIGN, ast, Int.MaxValue)
    }

    expect(TK_STRING, ast, 1)
  }

  def declare: AST = {
    val ast = expectAST(TK_DECLARE)

    rule(() => typeclass, ast, 0)
    rule(() => is, ast, 1)
    rule(() => declaremap, ast, 2)
  }

  def defType: AST = {
    val ast = expectAST(TK_TYPE)

    expect(TK_TYPEID, ast, 0)
    rule(() => oftype, ast, 1)
    rule(() => is, ast, 2)
  }

  def defTrait: AST = {
    val ast = expectAST(TK_TRAIT)

    expect(TK_TYPEID, ast, 0)
    rule(() => formalargs, ast, 1)
    rule(() => is, ast, 2)
    rule(() => typebody, ast, 3)

  }

  def defObject: AST = {
    val ast = expectAST(TK_OBJECT)

    expect(TK_TYPEID, ast, 0)
    rule(() => formalargs, ast, 1)
    rule(() => is, ast, 2)
    rule(() => typebody, ast, 3)
  }

  def actor: AST = {
    val ast = expectAST(TK_ACTOR)

    expect(TK_TYPEID, ast, 0)
    rule(() => formalargs, ast, 1)
    rule(() => is, ast, 2)
    rule(() => typebody, ast, 3)
  }

  def typeclass: AST = ???

  def is: AST = {
    val ast = newAST(TK_IS)

    if (accept(TK_IS, ast, SLOT_MAX)) {
      ruleList(() => typeclass, TK_COMMA, ast, 0)
    }

    ast
  }

  def oftype: AST = ???
  def formalargs: AST = ???
  def defVar: AST = ???
  def delegate: AST = ???
  def constructor: AST = ???
  def ambient: AST = ???
  def function: AST = ???
  def message: AST = ???

  def typebody: AST = {
    val alt: AltList = Seq(
      new Alt(TK_VAR, () => defVar),
      new Alt(TK_DELEGATE, () => delegate),
      new Alt(TK_NEW, () => constructor),
      new Alt(TK_AMBIENT, () => ambient),
      new Alt(TK_FUNCTION, () => function),
      new Alt(TK_MESSAGE, () => message)
    )

    val ast = newAST(TK_TYPEBODY)

    expect(TK_LBRACE, ast, SLOT_MAX)
    ruleAltList(alt, ast, 0)
    expect(TK_RBRACE, ast, SLOT_MAX)
  }

  def map: AST = {
    val ast = newAST(TK_MAP)

    expect(TK_ID, ast, 0)
    expect(TK_ASSIGN, ast, SLOT_MAX)
    expect(TK_ID, ast, 1)
  }

  def declaremap: AST = {
    val ast = newAST(TK_DECLAREMAP)

    if (accept(TK_LBRACE, ast, SLOT_MAX)) {
      ruleList(() => map, TK_COMMA, ast, 0)
      expect(TK_RBRACE, ast, SLOT_MAX)
    }

    ast
  }


  def rule(f: F, ast: AST, slot: Int): AST = ???
  def ruleList(f: F, tok: TokenType, ast: AST, slot: Int) {???}
  def expectAST(tok: TokenType): AST = ???
  def accept(tok: TokenType, ast: AST, slot: Int): Boolean = ???
  def newAST(tok: TokenType) = ???

  def ruleAltList(alt: AltList, ast: AST, slot: Int) = ???
  def expect(tok: TokenType, ast: AST, slot: Int): AST = ???

}

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