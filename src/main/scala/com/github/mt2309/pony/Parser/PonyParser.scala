package com.github.mt2309.pony.Parser

import scala.util.parsing.combinator.RegexParsers
import language.postfixOps

import com.github.mt2309.pony.AST._
import com.github.mt2309.pony.Common._

object PonyParser {

  def parse(file: (Filename, FileContents)): Option[Module] = new PonyParser(file._2)(file._1).parse
}

final class PonyParser(val contents: FileContents)(implicit val filename: Filename) extends RegexParsers {

  def parse: Option[Module] = {
    parseAll(module, contents) match {
      case Success(module, _ ) => {println(s"Parsing file: $filename was a success"); Some(module) }
      case e: NoSuccess => {println(s"failure in $filename\t${e.msg}\t${e.next.pos}"); None }
    }
  }

  // Module - top level of everything
  private def module: Parser[Module] = ((use*) ~ (namedMembers+)) ^^ {s => new Module(s._1.toSet, s._2.map(x => x.typeName -> x).toMap)}

  private def namedMembers: Parser[ModuleMember] = declare | traitParser | objectParser | actor | typeParser

  private def use: Parser[Use] = "use" ~> ((typeId <~ "=")?) ~ string ^^ {s => new Use(s._1, s._2)}

  private def declare: Parser[Declare] = "declare" ~> typeId ~ is ~ (declaremap?) ^^ {
    s => new Declare(s._1._1, s._1._2.getOrElse(new Is(List.empty)), s._2.getOrElse(new DeclareMap(List.empty)))
  }

  private def declaremap: Parser[DeclareMap] = "{" ~> parserList(map, ",") <~ "}" ^^ {new DeclareMap(_)}

  private def map: Parser[PonyMap] = id ~ "=" <~ id ^^ {s => new PonyMap(s._2, s._1)}

  private def typeParser: Parser[Type] = ("type" ~> typeId) ~ (ofType ~ is) ^^ {
    s => new Type(s._1, s._2._1, s._2._2.getOrElse(new Is(List.empty)))
  }

  private def actor: Parser[Actor] = ("actor" ~> typeId) ~ formalArgs ~ is ~ typeBody ^^ {
    s => new Actor(n = s._1._1._1, f = s._1._1._2.getOrElse(List.empty), i = s._1._2.getOrElse(new Is(List.empty)), t = s._2)
  }

  private def traitParser: Parser[Trait] = ("trait" ~> typeId) ~ formalArgs ~ is ~ typeBody ^^ {
    s => new Trait(n = s._1._1._1, f = s._1._1._2.getOrElse(List.empty), i = s._1._2.getOrElse(new Is(List.empty)), t = s._2)
  }
  private def objectParser: Parser[Object] = "object" ~> typeId ~ formalArgs ~ is ~ typeBody ^^ {
    s => new Object(n = s._1._1._1, f = s._1._1._2.getOrElse(List.empty), i = s._1._2.getOrElse(new Is(List.empty)), t = s._2)
  }

  private def is: Parser[Option[Is]] = ("is" ~> parserList(typeclass, ",")).? ^^ {s => s.map(new Is(_))}

  // Body Contents start here

  private def typeBody: Parser[TypeBody] = "{" ~> ((field | delegate | constructor | ambient | function | message)*) <~ "}" ^^ {
    s => new TypeBody(s.map(f => f.name -> f).toMap)
  }

  private def message: Parser[BodyContent] = "message" ~> methodContent ~ OptBlock ^^ {
    s => new Message(contents = s._1, block = s._2)
  }

  private def function: Parser[BodyContent] = "function" ~> methodContent ~ results ~ throws ~ OptBlock ^^ {
    s => new Function(contents = s._1._1._1, results = s._1._1._2, throws = s._1._2, block = s._2)
  }

  private def field: Parser[BodyContent] = "var" ~> id ~ ofType ~ fieldExpr ^^ {s => new Field(id = s._1._1, ofType = s._1._2, expr = s._2)}
  private def fieldExpr: Parser[Option[Expr]] = fieldSome | fieldNone
  private def fieldSome: Parser[Option[Expr]] = "=" ~> expr ^^ {Some(_)}
  private def fieldNone: Parser[Option[Expr]] = ";" ^^ {s => None}

  private def delegate: Parser[BodyContent] = "delegate" ~> id ~ ofType ^^ {s => new Delegate(s._1, s._2)}

  private def constructor: Parser[BodyContent] = "new" ~> methodContent ~ throws ~ OptBlock ^^ {
    s => new Constructor(contents = s._1._1, throws = s._1._2, block = s._2)
  }

  private def ambient: Parser[BodyContent] = "ambient" ~> methodContent ~ throws ~ OptBlock ^^ {
    s => new Ambient(contents = s._1._1, throws = s._1._2, block = s._2)
  }

  private def modeId: Parser[(Option[Mode], ID)] = (mode?) ~ id ^^ {s => s._1 -> s._2}
  private def combinedArgs: Parser[CombinedArgs] = formalArgs ~ params ^^ {s => new CombinedArgs(s._1.getOrElse(List.empty),s._2)}
  private def results: Parser[Option[Params]] = (("->" ~> params)?)

  private def params: Parser[Params] = "(" ~> (parserList(param, ",")?) <~ ")" ^^ {s => s.getOrElse(List.empty)}

  private def param: Parser[Param] = id ~ ofType ^^ {s => new Param(s._1, s._2)}

  private def methodContent: Parser[MethodContent] = modeId ~ combinedArgs ^^ {s => new MethodContent(s._1._1.getOrElse(ReadOnly), s._1._2, s._2)}

  private def OptBlock: Parser[Option[Block]] = (NoneBlock | SomeBlock) ^^ {s => s}
  private def SomeBlock: Parser[Option[Block]] = block ^^ {Some(_)}
  private def NoneBlock: Parser[Option[Block]] = ";" ^^ {s => None}

  private def ofType: Parser[OfType] = (":" ~> parserList(typeElement, "|")) ^^ {s => new OfType(s.toSet)}

  private def typeElement: Parser[TypeElement] = (partialType | typeclass | lambda) ^^ {case s: TypeElement => s}

  private def partialType: Parser[PartialType] = "\\" ~> typeclass ^^ {new PartialType(_)}

  private def typeclass: Parser[TypeClass] = typeId ~ optModule ~ ((mode?) ~ formalArgs) ^^ {
    s => new TypeClass(name = s._1._1, module = s._1._2, mode = s._2._1.getOrElse(ReadOnly), formalArgs = s._2._2.getOrElse(List.empty))
  }

  private def optModule: Parser[Option[TypeId]] = (("::" ~> typeId)?)

  private def lambda: Parser[Lambda] = "lambda" ~> (mode ~ args) ~ results ~ throws ~ isOptBlock ^^ {
    s => new Lambda(mode = s._1._1._1._1, args = s._1._1._1._2, result = s._1._1._2, throws = s._1._2, block = s._2)
  }

  private def isOptBlock: Parser[Option[Block]] = (("is" ~> block)?)

  private def throws: Parser[Boolean] = ("throws"?) ^^ {_.isDefined}

  private def formalArgs: Parser[Option[List[TypeClass]]] = (("[" ~> parserList(typeclass, ",") <~ "]")?)

  private def block: Parser[Block] = "{" ~> blockList ~ (catchBlock?) ~ (always?) <~ "}" ^^ {s => new Block(s._1._1, s._1._2, s._2)}

  private def blockList: Parser[List[BlockContent]] = ((block | conditional | forLoop | whileLoop | doLoop | matchParser | blockLiterals | assignment )*)

  private def blockLiterals: Parser[BlockContent] = returnLiteral | breakLiteral | continueLiteral | throwLiteral
  private def returnLiteral: Parser[BlockContent] = "return" ^^ {s => Return}
  private def breakLiteral: Parser[BlockContent] = "break" ^^ {s => Break}
  private def continueLiteral: Parser[BlockContent] = "continue" ^^ {s => Continue}
  private def throwLiteral: Parser[BlockContent] = "throw" ^^ {s => Throw}

  private def assignment: Parser[Assignment] = parserList(lvalue, ",") ~ (("=" ~> expr)?) ^^ {s => new Assignment(s._1, s._2)}

  private def lvalue: Parser[LValue] = lValueVar | lValueCommand
  private def lValueVar: Parser[LValue] = varDec ^^ {new LValueVar(_)}
  private def lValueCommand: Parser[LValue] = command ^^ {new LValueCommand(_)}

  private def varDec: Parser[VarDec] = "var" ~> id ~ (ofType?) ~ (("=" ~> expr)?) ^^ {s => new VarDec(s._1._1, s._1._2, s._2)}

  private def catchBlock: Parser[Block] = "catch" ~> block

  private def always: Parser[Block] = "always" ~> block

  private def matchParser: Parser[BlockContent] = ("match" ~> parserList(expr, ",")) ~ ("{" ~> (caseBlock+) <~ "}") ^^ {s => new Match(s._1, s._2)}

  private def doLoop: Parser[BlockContent] = ("do" ~> block) ~ ("while" ~> expr) ^^ {s => new DoLoop(s._1, s._2)}
  private def whileLoop: Parser[BlockContent] = "while" ~> expr ~ block ^^ {s => new WhileLoop(s._1, s._2)}
  private def forLoop: Parser[BlockContent] = ("for" ~> parserList(forVar, ",")) ~ ("in" ~> expr ~ block) ^^ {s => new ForLoop(s._1, s._2._1, s._2._2)}

  private def caseBlock: Parser[CaseBlock] = "case" ~> (caseSubBlock?) ~ block ^^ {s => new CaseBlock(s._1, s._2)}

  private def caseSubBlock: Parser[CaseSubBlock] = caseIf | caseVarList
  private def caseIf: Parser[CaseSubBlock] = "if" ~> expr ^^ {new CaseIf(_)}
  private def caseVarList: Parser[CaseSubBlock] = parserList(casevar, ",") ^^ {new CaseVarList(_)}

  private def casevar: Parser[CaseVar] = (expr?) ~ ("as" ~> forVar) ^^ {s => new CaseVar(s._1, s._2)}

  private def forVar: Parser[ForVar] = id ~ (ofType?) ^^ {s => new ForVar(s._1, s._2)}

  // Something about this seems off, might cause problems with conditionals
  private def conditional: Parser[BlockContent] = ("if" ~> expr ~ block) ~ (("else if" ~> expr ~ block)*) ~ (("else" ~> block)?) ^^ {
    s => new Conditional((s._1._1._1 -> s._1._1._2) :: s._1._2.map(x => x._1 -> x._2), s._2)
  }

  private def args: Parser[List[Arg]] = "(" ~> parserList(arg, ",") <~ ")"
  private def arg: Parser[Arg] = (expr?) ~ (ofType?) ~ (("=" ~> expr)?) ^^ {s => new Arg(s._1._1, s._1._2, s._2)}

  private def expr: Parser[Expr] = unary ~ ((operator ~ unary)*) ^^ {s => new Expr(s._1, s._2.map(t => t._1 -> t._2))}

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
  private def command: Parser[Command] = firstCommand ~ (secondCommand?) ^^ {s => new Command(s._1, s._2)}

  private def firstCommand: Parser[FirstCommand] = commandExpr | commandArgs | atom
  private def commandExpr: Parser[CommandExpr] = "(" ~> expr <~ ")" ^^ {new CommandExpr(_)}
  private def commandArgs: Parser[CommandArgs] = "[" ~> parserList(arg, ",") <~ "]" ^^ {new CommandArgs(_)}

  private def secondCommand: Parser[SecondCommand] = secondCommandArgs | commandCall
  private def secondCommandArgs: Parser[SecondCommand] = args ^^ {new SecondCommandArgs(_)}

  private def commandCall: Parser[SecondCommand] = "." ~> id ~ formalArgs ~ args ^^ {
    s => new CommandCall(s._1._1, s._1._2.getOrElse(List.empty), s._2)
  }

  // Atoms
  private def atom: Parser[Atom] = thisParse | trueParse | falseParse | intParse | floatParse | stringParse | idParse | typeIdParse
  private def thisParse: Parser[Atom] = "this" ^^ {s => This}
  private def trueParse: Parser[Atom] = "true" ^^ {s => True}
  private def falseParse: Parser[Atom] = "false" ^^ {s => False}
  private def intParse: Parser[Atom] = int ^^ {new PonyInt(_)}
  private def floatParse: Parser[Atom] = double ^^ {new PonyDouble(_)}
  private def stringParse: Parser[Atom] = string ^^ {new PonyString(_)}
  private def idParse: Parser[Atom] = id ^^ {new PonyID(_)}
  private def typeIdParse: Parser[Atom] = typeId ^^ {new PonyTypeId(_)}


  // Modes
  private def mode: Parser[Mode] = immutable | mutable | unique | modeExpr
  private def immutable: Parser[Mode] = "!" ^^ {s => Immutable}
  private def mutable: Parser[Mode] = "~" ^^ {s => Mutable}
  private def unique: Parser[Mode] = "@" ^^ {s => Unique}
  private def modeExpr: Parser[ModeExpr] = "[:" ~> expr <~ "]" ^^ {new ModeExpr(_)}

  // Basic types
  private def typeId: Parser[TypeId] = """[A-Z][a-zA-Z_0-9]*""".r
  private def string: Parser[String] = ("\""+"""([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*"""+"\"").r
  private def int: Parser[Int] = """-?\d+""".r ^^ { _.toInt }
  private def double: Parser[Double] = """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r ^^ {_.toDouble}
  private def id: Parser[ID] = """[a-z][a-zA-Z_0-9]*""".r

  // Helper
  private def parserList[K](p: Parser[K], tok: String): Parser[List[K]] = ((p ~ tok)*) ~ p ^^ {s => s._1.map(_._1) ++ List(s._2)}

  override protected val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r
}