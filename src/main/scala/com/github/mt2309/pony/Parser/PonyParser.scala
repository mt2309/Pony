package com.github.mt2309.pony.Parser

import language.postfixOps

import com.github.mt2309.pony.AST._
import com.github.mt2309.pony.Common._
import org.kiama.util.ParserUtilities

object PonyParser {

  def parse(file: (Filename, FileContents)): Option[Module] = new PonyParser(file._2)(file._1).parse
}

final class PonyParser(val contents: FileContents)(implicit val filename: Filename) extends ParserUtilities {

  def parse: Option[Module] = {
    parseAll(module, contents) match {
      case Success(module, _ ) => Some(module)
      case e: NoSuccess => {println(s"failure in $filename\t${e.msg}\t${e.next.pos}"); None }
    }
  }

  // Module - top level of everything
  private def module: Parser[Module] = positioned(use.* ~ namedMembers.+ ^^ {s => new Module(s._1.toSet, s._2.map(x => x.typeName -> x).toMap)})

  private def namedMembers: Parser[ModuleMember] = positioned {
    declare | traitParser | objectParser | actor | typeParser
  }

  private def use: Parser[Use] = positioned {
    "use" ~> ((typeId <~ "=")?) ~ string ^^ {
      s => new Use(s._1, s._2)
    }
  }

  private def declare: Parser[Declare] = positioned {
    "declare" ~> typeId ~ is ~ declaremap.? ^^ {
      s => new Declare(s._1._1, s._1._2, s._2.getOrElse(new DeclareMap(List.empty)))
    }
  }

  private def declaremap: Parser[DeclareMap] = positioned {
    "{" ~> parserList(map, ",") <~ "}" ^^ {new DeclareMap(_)}
  }

  private def map: Parser[PonyMap] = positioned {
    id ~ "=" ~ id ^^ {s => new PonyMap(s._2, s._1._1)}
  }

  private def typeParser: Parser[Type] = positioned {
    "type" ~> typeId ~ (ofType ~ is) ^^ { s => new Type(s._1, s._2._1, s._2._2)}
  }

  private def actor: Parser[Actor] = positioned {
    "actor" ~> typeId ~ formalParams ~ is ~ typeBody ^^ {
      s => new Actor(n = s._1._1._1, f = s._1._1._2, i = s._1._2, t = s._2)
    }
  }

  private def traitParser: Parser[Trait] = positioned {
    "trait" ~> typeId ~ formalParams ~ is ~ typeBody ^^ {
      s => new Trait(n = s._1._1._1, f = s._1._1._2, i = s._1._2, t = s._2)
    }
  }
  private def objectParser: Parser[Object] = positioned {
    ("static".? <~ "object") ~ (typeId ~ formalParams) ~ (is ~ typeBody) ^^ {
      s => new Object(isStatic = s._1._1.isDefined, n = s._1._2._1, f = s._1._2._2, i = s._2._1, t = s._2._2)
    }
  }

  private def is: Parser[Is] = positioned {
    ("is" ~> parserList(typeclass, ",")).? ^^ {s => new Is(s.getOrElse(List.empty))}
  }

  // Body Contents start here

  private def typeBody: Parser[TypeBody] = positioned {
    "{" ~> ((field | delegate | constructor | ambient | function | message)*) <~ "}" ^^ {
      s => new TypeBody(s.map(f => f.name -> f).toMap)
    }
  }

  private def message: Parser[BodyContent] = positioned {
    "message" ~> methodContent ~ OptBlock ^^ {
      s => new Message(contents = s._1, block = s._2)
    }
  }

  private def function: Parser[BodyContent] = positioned {
    "function" ~> methodContent ~ results ~ throws ~ OptBlock ^^ {
      s => new Function(contents = s._1._1._1, results = s._1._1._2, throws = s._1._2, block = s._2)
    }
  }

  private def field: Parser[BodyContent] = positioned {
    "var" ~> id ~ ofType ~ fieldExpr ^^ {s => new Field(id = s._1._1, ofType = s._1._2, expr = s._2)}
  }
  private def fieldExpr: Parser[Option[Expr]] = fieldSome | fieldNone
  private def fieldSome: Parser[Option[Expr]] = "=" ~> expr ^^ {Some(_)}
  private def fieldNone: Parser[Option[Expr]] = ";" ^^ {s => None}

  private def delegate: Parser[BodyContent] = positioned {
    "delegate" ~> id ~ ofType ^^ {s => new Delegate(s._1, s._2)}
  }

  private def constructor: Parser[BodyContent] = positioned {
    "new" ~> methodContent ~ throws ~ OptBlock ^^ {
      s => new Constructor(contents = s._1._1, throws = s._1._2, block = s._2)
    }
  }

  private def ambient: Parser[BodyContent] = positioned {
    "ambient" ~> methodContent ~ throws ~ OptBlock ^^ {
      s => new Ambient(contents = s._1._1, throws = s._1._2, block = s._2)
    }
  }

  private def modeId: Parser[(Option[Mode], ID)] = (mode?) ~ id ^^ {s => s._1 -> s._2}
  private def combinedArgs: Parser[CombinedArgs] = positioned {
    formalParams ~ params ^^ {s => new CombinedArgs(s._1,s._2)}
  }
  private def results: Parser[Params] = (("->" ~> params)?) ^^ {_.getOrElse(List.empty)}

  private def params: Parser[Params] = "(" ~> (parserList(param, ",")?) <~ ")" ^^ {s => s.getOrElse(List.empty)}

  private def param: Parser[Param] = positioned {id ~ ofType ^^ {s => new Param(s._1, s._2)}}

  private def methodContent: Parser[MethodContent] = positioned {
    modeId ~ combinedArgs ^^ {s => new MethodContent(s._1._1.getOrElse(ReadOnly), s._1._2, s._2)}
  }

  private def OptBlock: Parser[Option[Block]] = NoneBlock | SomeBlock
  private def SomeBlock: Parser[Option[Block]] = block ^^ {Some(_)}
  private def NoneBlock: Parser[Option[Block]] = ";" ^^ {s => None}

  private def ofType: Parser[OfType] = concreteOf | thisOf

  private def thisOf: Parser[OfType] = positioned(":" ~ "this.type" ^^ {s => new ThisOfType})

  private def concreteOf: Parser[OfType] =  positioned {
    ":" ~> parserList(typeElement, "|") ^^ {s => new ConcreteOfType(s.toSet)}
  }

  private def typeElement: Parser[TypeElement] = positioned {
    partialType | typeclass | lambda
  }

  private def partialType: Parser[PartialType] = "\\" ~> typeclass ^^ {new PartialType(_)}

  private def typeclass: Parser[TypeClass] = positioned {
    typeId ~ optModule ~ ((mode?) ~ formalArgs) ^^ {
      s => new TypeClass(name = s._1._1, module = s._1._2, mode = s._2._1.getOrElse(ReadOnly), formalArgs = s._2._2.getOrElse(List.empty))
    }
  }

  private def optModule: Parser[Option[TypeId]] = ("::" ~> typeId).?

  private def lambda: Parser[Lambda] = positioned {
    "lambda" ~> (mode ~ args) ~ results ~ throws ~ isOptBlock ^^ {
      s => new Lambda(mode = s._1._1._1._1, args = s._1._1._1._2, result = s._1._1._2, throws = s._1._2, block = s._2)
    }
  }

  private def isOptBlock: Parser[Option[Block]] = ("is" ~> block).?

  private def throws: Parser[Boolean] = ("throws"?) ^^ {_.isDefined}

  private def formalParams: Parser[List[TypeId]] = (("[" ~> parserList(typeId, ",") <~ "]")?) ^^ {_.getOrElse(List.empty)}

  private def formalArgs: Parser[Option[List[TypeClass]]] = ("[" ~> parserList(typeclass, ",") <~ "]").?

  private def block: Parser[Block] = positioned {
    "{" ~> blockList ~ (catchBlock?) ~ (always?) <~ "}" ^^ {s => new Block(s._1._1, s._1._2, s._2)}
  }

  private def blockList: Parser[List[BlockContent]] = (forLoop |block | conditional |  whileLoop | doLoop | matchParser | blockLiterals | assignment).*

  private def blockLiterals: Parser[BlockContent] = positioned {
    returnLiteral | breakLiteral | continueLiteral | throwLiteral
  }
  private def returnLiteral: Parser[BlockContent] = positioned {
    "return" ^^ {s => Return}
  }
  private def breakLiteral: Parser[BlockContent] = positioned {
    "break" ^^ {s => Break}
  }
  private def continueLiteral: Parser[BlockContent] = positioned {
    "continue" ^^ {s => Continue}
  }
  private def throwLiteral: Parser[BlockContent] = positioned {
    "throw" ^^ {s => Throw}
  }

  private def assignment: Parser[Assignment] = positioned {
    parserList(lvalue, ",") ~ (("=" ~> expr)?) ^^ {s => new Assignment(s._1, s._2)}
  }

  private def lvalue: Parser[LValue] = positioned {
    lValueVar | lValueCommand
  }

  private def lValueVar: Parser[LValue] = positioned {
    varDec ^^ {new LValueVar(_)}
  }
  private def lValueCommand: Parser[LValue] = positioned {
    command ^^ {new LValueCommand(_)}
  }

  private def varDec: Parser[VarDec] = positioned {
    "var" ~> id ~ (ofType?) ~ (("=" ~> expr)?) ^^ {s => new VarDec(s._1._1, s._1._2, s._2)}
  }

  private def catchBlock: Parser[Block] = positioned {"catch" ~> block}

  private def always: Parser[Block] = positioned {"always" ~> block}

  private def matchParser: Parser[BlockContent] = positioned {
    "match" ~> parserList(expr, ",") ~ ("{" ~> (caseBlock+) <~ "}") ^^ {s => new Match(s._1, s._2)}
  }

  private def doLoop: Parser[BlockContent] = positioned {
    "do" ~> block ~ ("while" ~> bracketedExpr) ^^ {s => new DoLoop(s._1, s._2)}
  }
  private def whileLoop: Parser[BlockContent] = positioned {
    "while" ~> bracketedExpr ~ block ^^ {s => new WhileLoop(s._1, s._2)}
  }
  private def forLoop: Parser[BlockContent] = positioned {
    "for" ~> parserList(forVar, ",") ~ ("in" ~> expr ~ block) ^^ {s => new ForLoop(s._1, s._2._1, s._2._2)}
  }

  private def caseBlock: Parser[CaseBlock] = positioned {
    "case" ~> (caseSubBlock?) ~ block ^^ {s => new CaseBlock(s._1, s._2)}
  }

  private def caseSubBlock: Parser[CaseSubBlock] = positioned {
    caseIf | caseVarList
  }
  private def caseIf: Parser[CaseSubBlock] = positioned {
    "if" ~> bracketedExpr ^^ {new CaseIf(_)}
  }
  private def caseVarList: Parser[CaseSubBlock] = positioned {
    parserList(casevar, ",") ^^ {new CaseVarList(_)}
  }

  private def casevar: Parser[CaseVar] = positioned {
    (expr?) ~ ("as" ~> forVar) ^^ {s => new CaseVar(s._1, s._2)}
  }

  private def forVar: Parser[ForVar] = positioned {
    id ~ (ofType?) ^^ {s => new ForVar(s._1, s._2)}
  }

  // Something about this seems off, might cause problems with conditionals
  private def conditional: Parser[BlockContent] = positioned{
    "if" ~> bracketedExpr ~ block ~ (("else if" ~> bracketedExpr ~ block)*) ~ (("else" ~> block)?) ^^ {
      s => new Conditional(s._1._1._1 -> s._1._1._2 :: s._1._2.map(x => x._1 -> x._2), s._2)
    }
  }

  private def bracketedExpr: Parser[Expr] = positioned("(" ~> expr <~ ")")

  private def args: Parser[List[Arg]] = "(" ~> parserList(arg, ",") <~ ")"
  private def arg: Parser[Arg] = positioned {
    (expr?) ~ (ofType?) ~ (("=" ~> expr)?) ^^ {s => new Arg(s._1._1, s._1._2, s._2)}
  }

  private def expr: Parser[Expr] = positioned {
    unary ~ ((operator ~ unary)*) ^^ {s => new Expr(s._1, s._2.map(t => t._1 -> t._2))}
  }

  // Binary operators
  private def operator: Parser[Operator] = positioned {
    plus | minus | times | divide | mod | lshift | rshift | gt | eq | lt | gte | lte | ne | steq | stneq | or | xor | and
  }

  private def plus: Parser[Operator] = "+" ^^ {s => new Plus}
  private def minus: Parser[Operator] = "-" ^^ {s => new Minus}
  private def times: Parser[Operator] = "*" ^^ {s => new Times}
  private def divide: Parser[Operator] = "/" ^^ {s => new Divide}
  private def mod: Parser[Operator] = "%" ^^ {s => new Mod}
  private def lshift: Parser[Operator] = "<<" ^^ {s => new LShift}
  private def rshift: Parser[Operator] = ">>" ^^ {s => new RShift}
  private def gt: Parser[Operator] = ">" ^^ {s => new GT}
  private def lt: Parser[Operator] = "<" ^^ {s => new LT}
  private def gte: Parser[Operator] = ">=" ^^ {s => new GTE}
  private def lte: Parser[Operator] = "<=" ^^ {s => new LTE}
  private def eq: Parser[Operator] = "==" ^^ {s => new EQ}
  private def ne: Parser[Operator] = "!=" ^^ {s => new NE}
  private def steq: Parser[Operator] = "#=" ^^ {s => new STEq}
  private def stneq: Parser[Operator] = "~=" ^^ {s => new STNeq}
  private def or: Parser[Operator] = "|" ^^ {s => new Or}
  private def xor: Parser[Operator] = "^" ^^ {s => new XOr}
  private def and: Parser[Operator] = "&" ^^ {s => new And}

  // Unary operators
  private def unary: Parser[Unary] = positioned {
    unaryLambda | unaryCommand
  }

  private def unaryLambda: Parser[Unary] = positioned {
    unaryOp ~ lambda ^^ {s => new UnaryLambda(s._1, s._2)}
  }

  private def unaryCommand: Parser[Unary] = positioned {
    unaryOp ~ command ^^ {s => new UnaryCommand(s._1, s._2)}
  }

  private def unaryOp: Parser[List[UnaryOp]] = (partial | unaryMinus | unaryBang).*
  private def partial: Parser[UnaryOp] = positioned {
    "\\" ^^ {s => PARTIAL}
  }

  private def unaryMinus: Parser[UnaryOp] = positioned {
    "-" ^^ {s => UNARY_MINUS}
  }

  private def unaryBang: Parser[UnaryOp] = positioned {
    "!" ^^ {s => UNARY_BANG}
  }


  //Commands
  private def command: Parser[Command] = positioned {
    firstCommand ~ (secondCommand?) ^^ {s => new Command(s._1, s._2)}
  }

  private def firstCommand: Parser[FirstCommand] = positioned {
    commandExpr | commandArgs | atom
  }
  private def commandExpr: Parser[CommandExpr] = positioned {
    "(" ~> expr <~ ")" ^^ {new CommandExpr(_)}
  }
  private def commandArgs: Parser[CommandArgs] = positioned {
    "[" ~> parserList(arg, ",") <~ "]" ^^ {new CommandArgs(_)}
  }

  private def secondCommand: Parser[SecondCommand] = positioned{
    secondCommandArgs | commandCall
  }
  private def secondCommandArgs: Parser[SecondCommand] = positioned{
    args ^^ {new SecondCommandArgs(_)}
  }

  private def commandCall: Parser[SecondCommand] = positioned{
    "." ~> id ~ formalArgs ~ args ^^ {
      s => new CommandCall(s._1._1, s._1._2.getOrElse(List.empty), s._2)
    }
  }

  // Atoms
  private def atom: Parser[Atom] = positioned {
    thisParse | trueParse | falseParse | intParse | floatParse | stringParse | idParse | typeIdParse
  }
  private def thisParse: Parser[Atom] = "this" ^^ {s => This}
  private def trueParse: Parser[Atom] = "true" ^^ {s => True}
  private def falseParse: Parser[Atom] = "false" ^^ {s => False}
  private def intParse: Parser[Atom] = int ^^ {new PonyInt(_)}
  private def floatParse: Parser[Atom] = double ^^ {new PonyDouble(_)}
  private def stringParse: Parser[Atom] = string ^^ {new PonyString(_)}
  private def idParse: Parser[Atom] = id ^^ {new PonyID(_)}
  private def typeIdParse: Parser[Atom] = typeId ^^ {new PonyTypeId(_)}


  // Modes
  private def mode: Parser[Mode] = positioned {immutable | mutable | unique | modeExpr}

  private def immutable: Parser[Mode] = ("[:" ~ "imm" ~ "]" | "!") ^^ {s => Immutable}
  private def mutable: Parser[Mode]   = ("[:" ~ "mut" ~ "]" | "~") ^^ {s => Mutable}
  private def unique: Parser[Mode]    = ("[:" ~ "uniq" ~ "]" | "@") ^^ {s => Unique}
  private def modeExpr: Parser[ModeExpr] = positioned {
    "[:" ~> expr <~ "]" ^^ {new ModeExpr(_)}
  }

  // Basic types
  private def typeId: Parser[TypeId] = """[A-Z][a-zA-Z_0-9]*""".r ^^ {new TypeId(_)}
  private def string: Parser[String] = ("\""+"""([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*"""+"\"").r
  private def int: Parser[Int] = """-?\d+""".r ^^ { _.toInt }
  private def double: Parser[Double] = """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r ^^ {_.toDouble}
  private def id: Parser[ID] = """[a-z][a-zA-Z_0-9]*""".r ^^ {new ID(_)}

  // Helper
  private def parserList[K](p: Parser[K], tok: String): Parser[List[K]] = ((p ~ tok)*) ~ p ^^ {s => s._1.map(_._1) ++ List(s._2)}

  override protected val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r
}
