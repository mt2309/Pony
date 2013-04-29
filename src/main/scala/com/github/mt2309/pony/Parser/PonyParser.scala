package com.github.mt2309.pony.Parser

import scala.util.parsing.combinator.RegexParsers
import language.postfixOps

import com.github.mt2309.pony.AST._

object PonyParser extends RegexParsers {

  def parse(file: (String, String)) {
    parseAll(module, file._2) match {
      case Success(_, _) => println("success")
      case e: NoSuccess => println("failure in " + file._1 + "\t" + e.msg + "\t" + e.next.pos)
    }
  }

  // Module - top level of everything
  private def module: Parser[Module] = ((use | declare | typeParser | traitParser | objectParser | actor)*) ^^ {s => new Module(s)}

  private def use: Parser[Use] = "use" ~ ((typeId ~ "=")?) ~ string ^^ {s => new Use(s._1._2.map(_._1), s._2)}

  private def declare: Parser[Declare] = "declare" ~ typeclass ~ is ~ declaremap ^^ {s => new Declare(s._1._1._2, s._1._2, s._2)}

  private def declaremap: Parser[DeclareMap] = "{" ~ parserList(map, ",") ~ "}" ^^ {s => new DeclareMap(s._1._2)}

  private def map: Parser[PonyMap] = id ~ "=" ~ id ^^ {s => new PonyMap(s._2, s._1._1)}

  private def typeParser: Parser[Type] = ("type" ~ typeId) ~ (ofType ~ is) ^^ {s => new Type(s._1._2, s._2._1, s._2._2)}

  private def actor: Parser[Actor] = ("actor" ~ typeId) ~ formalargs ~ is ~ typeBody ^^ {
    s => new Actor(name = s._1._1._1._2, formalArgs = s._1._1._2, is = s._1._2, typeBody = s._2)
  }

  private def traitParser: Parser[Trait] = ("trait" ~ typeId) ~ formalargs ~ is ~ typeBody ^^ {
    s => new Trait(name = s._1._1._1._2, formalArgs = s._1._1._2, is = s._1._2, typeBody = s._2)
  }
  private def objectParser: Parser[Object] = "object" ~ typeId ~ formalargs ~ is ~ typeBody ^^ {
    s => new Object(name = s._1._1._1._2, formalArgs = s._1._1._2, is = s._1._2, typeBody = s._2)
  }

  private def is: Parser[Option[Is]] = ("is" ~ parserList(typeclass, ",")).? ^^ {s => s.map(t => new Is(t._2))}

  // Body Contents start here

  private def typeBody: Parser[TypeBody] = "{" ~ ((field | delegate | constructor | ambient | function | message)*) ~ "}" ^^ {s => new TypeBody(s._1._2)}

  private def message: Parser[Message] = "message" ~ methodContent ~ OptBlock ^^ {
    s => new Message(contents = s._1._2, block = s._2)
  }

  private def function: Parser[Function] = "function" ~ methodContent ~ results ~ throws ~ OptBlock ^^ {
    s => new Function(contents = s._1._1._1._2, results = s._1._1._2, throws = s._1._2, block = s._2)
  }

  private def field: Parser[Field] = "var" ~ id ~ ofType ~ (("=" ~ expr)?) ^^ {s => new Field(id = s._1._1._2, ofType = s._1._2, expr = s._2.map(_._2))}

  private def delegate: Parser[Delegate] = "delegate" ~ id ~ ofType ^^ {s => new Delegate(s._1._2, s._2)}

  private def constructor: Parser[Constructor] = "new" ~ methodContent ~ throws ~ OptBlock ^^ {
    s => new Constructor(contents = s._1._1._2, throws = s._1._2, block = s._2)
  }

  private def ambient: Parser[Ambient] = "ambient" ~ methodContent ~ throws ~ OptBlock ^^ {
    s => new Ambient(contents = s._1._1._2, throws = s._1._2, block = s._2)
  }

  private def modeId: Parser[(Option[Mode], ID)] = (mode?) ~ id ^^ {s => s._1 -> s._2}
  private def combinedArgs: Parser[CombinedArgs] = formalargs ~ args ^^ {s => new CombinedArgs(s._1,s._2)}
  private def results: Parser[Option[Args]] = (("->" ~ args)?) ^^ {s => s.map(_._2)}

  private def methodContent: Parser[MethodContent] = modeId ~ combinedArgs ^^ {s => new MethodContent(s._1._1, s._1._2, s._2)}

  private def OptBlock: Parser[Option[Block]] = (NoneBlock | SomeBlock) ^^ {s => s}
  private def SomeBlock: Parser[Option[Block]] = block ^^ {s => Some(s)}
  private def NoneBlock: Parser[Option[Block]] = ";" ^^ {s => None}

  private def ofType: Parser[OfType] = (":" ~ parserList(typeElement, "|")).? ^^ {s => new OfType(s.map(_._2))}

  private def typeElement: Parser[TypeElement] = (partialType | typeclass | lambda) ^^ {case s: TypeElement => s}

  private def partialType: Parser[PartialType] = "\\" ~ typeclass ^^ {s => new PartialType(s._2)}

  private def typeclass: Parser[TypeClass] = typeId ~ optModule ~ ((mode?) ~ formalargs) ^^ {
    s => new TypeClass(name = s._1._1, module = s._1._2, mode = s._2._1, formalArgs = s._2._2)
  }

  private def optModule: Parser[Option[TypeId]] = (("::" ~ typeId)?) ^^ {s => s.map(_._2)}

  private def lambda: Parser[Lambda] = "lambda" ~ (mode ~ args) ~ results ~ throws ~ isOptBlock ^^ {
    s => new Lambda(mode = s._1._1._1._2._1, args = s._1._1._1._2._2, result = s._1._1._2, throws = s._1._2, block = s._2)
  }

  private def isOptBlock: Parser[Option[Block]] = (("is" ~ block)?) ^^ {s => s.map(_._2)}

  private def throws: Parser[Boolean] = ("throws"?) ^^ {_.isDefined}

  private def formalargs: Parser[Option[List[Arg]]] = (("[" ~ parserList(arg, ",") ~ "]")?) ^^ {s => s.map(t => t._1._2)}

  private def block: Parser[Block] = { "{" ~
    blockList ~ (catchBlock?) ~ (always?) ~ "}" ^^ {s => new Block(s._1._1._1._2, s._1._1._2, s._1._2)}
  }

  private def blockList: Parser[List[BlockContent]] = ((block | conditional | forLoop | whileLoop | doLoop | matchParser | blockLiterals | assignment )*)

  private def blockLiterals: Parser[BlockContent] = returnLiteral | breakLiteral | continueLiteral | throwLiteral
  private def returnLiteral: Parser[BlockContent] = "return" ^^ {s => Return}
  private def breakLiteral: Parser[BlockContent] = "break" ^^ {s => Break}
  private def continueLiteral: Parser[BlockContent] = "continue" ^^ {s => Continue}
  private def throwLiteral: Parser[BlockContent] = "throw" ^^ {s => Throw}

  private def assignment: Parser[Assignment] = parserList(lvalue, ",") ~ (("=" ~ expr)?) ^^ {s => new Assignment(s._1, s._2.map(_._2))}

  private def lvalue: Parser[LValue] = lValueVar | lValueCommand
  private def lValueVar: Parser[LValue] = varDec ^^ {new LValueVar(_)}
  private def lValueCommand: Parser[LValue] = command ^^ {new LValueCommand(_)}

  private def varDec: Parser[VarDec] = "var" ~ id ~ ofType ^^ {s => new VarDec(s._1._2, s._2)}

  private def catchBlock: Parser[Block] = "catch" ~ block ^^ {_._2}

  private def always: Parser[Block] = "always" ~ block ^^ {_._2}

  private def matchParser: Parser[BlockContent] = "match" ~ parserList(expr, ",") ~ "{" ~ (caseBlock+) ~ "}" ^^ {s => new Match(s._1._1._1._2, s._1._2)}

  private def caseBlock: Parser[CaseBlock] = "case" ~ ((("if" ~ expr) | parserList(casevar, ","))?) ~ block ^^ {s => new CaseBlock(s._1._2, s._2)}

  private def casevar: Parser[CaseVar] = (expr?) ~ ("as" ~ forVar) ^^ {s => new CaseVar(s._1, s._2._2)}

  private def doLoop: Parser[BlockContent] = ("do" ~ block) ~ ("while" ~ expr) ^^ {s => new DoLoop(s._1._2, s._2._2)}
  private def whileLoop: Parser[BlockContent] = "while" ~ expr ~ block ^^ {s => new WhileLoop(s._1._2, s._2)}
  private def forLoop: Parser[BlockContent] = ("for" ~ parserList(forVar, ",")) ~ ("in" ~ expr ~ block) ^^ {s => new ForLoop(s._1._2, s._2._1._2, s._2._2)}

  private def forVar: Parser[ForVar] = id ~ ofType ^^ {s => new ForVar(s._1, s._2)}

  // Something about this seems off, might cause problems with conditionals
  private def conditional: Parser[BlockContent] = ("if" ~ expr ~ block) ~ (("else if" ~ expr ~ block)*) ~ (("else" ~ block)?) ^^ {
    s => new Conditional((s._1._1._1._2 -> s._1._1._2) :: s._1._2.map(x => x._1._2 -> x._2), s._2.map(_._2))}

  private def args: Parser[List[Arg]] = "(" ~ parserList(arg, ",") ~ ")" ^^ {s => s._1._2}
  private def arg: Parser[Arg] = expr ~ ofType ~ (("=" ~ expr)?) ^^ {s => new Arg(s._1._1, s._1._2, s._2 match {case Some(x) => Some(x._2); case None => None})}

  private def expr: Parser[Expr] = unary ~ ((operator ~ unary)?) ^^ {s => new Expr(s._1, s._2.map(t => t._1 -> t._2))}

  // Binary operators
  private def operator: Parser[Operator] = plus | minus | times | divide | mod | lshift | rshift | gt | lt | gte | lte | ne | steq | nsteq | or | xor | and
  private def plus: Parser[Operator] = "+" ^^ {s => PLUS}
  private def minus: Parser[Operator] = "-" ^^ {s => MINUS}
  private def times: Parser[Operator] = "*" ^^ {s => TIMES}
  private def divide: Parser[Operator] = "/" ^^ {s => DIVIDE}
  private def mod: Parser[Operator] = "%" ^^ {s => MOD}
  private def lshift: Parser[Operator] = "<<" ^^ {s => LSHIFT}
  private def rshift: Parser[Operator] = ">>" ^^ {s => RSHIFT}
  private def gt: Parser[Operator] = ">" ^^ {s => GT}
  private def lt: Parser[Operator] = "<" ^^ {s => LT}
  private def gte: Parser[Operator] = ">=" ^^ {s => GTE}
  private def lte: Parser[Operator] = "<=" ^^ {s => LTE}
  private def ne: Parser[Operator] = "!=" ^^ {s => NE}
  private def steq: Parser[Operator] = "#=" ^^ {s => STEQ}
  private def nsteq: Parser[Operator] = "~=" ^^ {s => NSTEQ}
  private def or: Parser[Operator] = "|" ^^ {s => OR}
  private def xor: Parser[Operator] = "^" ^^ {s => XOR}
  private def and: Parser[Operator] = "&" ^^ {s => AND}

  // Unary operators
  private def unary: Parser[Unary] = unaryLambda | unaryCommand
  private def unaryLambda: Parser[Unary] = unaryOp ~ lambda ^^ {s => new UnaryLambda(s._1, s._2)}
  private def unaryCommand: Parser[Unary] = unaryOp ~ command ^^ {s => new UnaryCommand(s._1, s._2)}

  private def unaryOp: Parser[List[UnaryOp]] = ((partial | unaryMinus | unaryBang)*)
  private def partial: Parser[UnaryOp] = "\\" ^^ {s => PARTIAL}
  private def unaryMinus: Parser[UnaryOp] = "-" ^^ {s => UNARY_MINUS}
  private def unaryBang: Parser[UnaryOp] = "!" ^^ {s => UNARY_BANG}


  //Commands
  private def command: Parser[Command] = firstCommand ~ secondCommand ^^ {s => new Command(s._1, s._2)}

  private def firstCommand: Parser[FirstCommand] = commandExpr | commandArgs | atom
  private def commandExpr: Parser[CommandExpr] = "(" ~ expr ~ ")" ^^ {s => new CommandExpr(s._1._2)}
  private def commandArgs: Parser[CommandArgs] = "[" ~ parserList(arg, ",") ~ "]" ^^ {s => new CommandArgs(s._1._2)}

  private def secondCommand: Parser[SecondCommand] = secondCommandArgs | commandCall
  private def secondCommandArgs: Parser[SecondCommand] = args ^^ {new SecondCommandArgs(_)}
  private def commandCall: Parser[SecondCommand] = "." ~ id ~ formalargs ~ args ^^ {s => new CommandCall(s._1._1._2, s._1._2, s._2)}

  // Atoms
  private def atom: Parser[Atom] = thisParse | trueParse | falseParse | intParse | stringParse | idParse | typeIdParse
  private def thisParse: Parser[Atom] = "this" ^^ {s => This}
  private def trueParse: Parser[Atom] = "true" ^^ {s => True}
  private def falseParse: Parser[Atom] = "false" ^^ {s => False}
  private def intParse: Parser[Atom] = int ^^ {s => new PonyInt(s)}
  private def stringParse: Parser[Atom] = string ^^ {new PonyString(_)}
  private def idParse: Parser[Atom] = id ^^ {new PonyID(_)}
  private def typeIdParse: Parser[Atom] = typeId ^^ {new PonyTypeId(_)}


  // Modes
  private def mode: Parser[Mode] = immutable | mutable | unique | modeExpr ^^ {s => s}
  private def immutable: Parser[Mode] = "!" ^^ {s => Immutable}
  private def mutable: Parser[Mode] = "~" ^^ {s => Mutable}
  private def unique: Parser[Mode] = "@" ^^ {s => Unique}
  private def modeExpr: Parser[ModeExpr] = "[:" ~ expr ~ "]" ^^ {s => new ModeExpr(s._1._2)}

  // Basic types
  private def typeId: Parser[TypeId] = """[A-Z][a-zA-Z_]*""".r
  private def string: Parser[String] = ("\""+"""([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*"""+"\"").r
  private def int: Parser[Int] = """-?\d+""".r ^^ { _.toInt }
  private def float: Parser[Double] = """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r ^^ {_.toDouble}
  private def id: Parser[ID] = """[a-z][a-zA-Z_]*""".r

  // Helper
  private def parserList[K](p: Parser[K], tok: String): Parser[List[K]] = ((p ~ tok)*) ~ p ^^ {s => s._1.map(_._1) ++ List(s._2)}
}