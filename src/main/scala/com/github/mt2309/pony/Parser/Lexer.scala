package com.github.mt2309.pony.Parser

import com.github.mt2309.pony.AST._
import com.github.mt2309.pony.Error.CompilerError
import util.control.Breaks
import collection.immutable.StringOps

/**
 * User: mthorpe
 * Date: 26/04/2013
 * Time: 00:49
 */

final case class Symbol_T(symbol: String, id: TokenType)

final class LexerException(msg:String) extends Exception(msg)

final class Lexer(val filename: String, val file: String, val errorList: ErrorList) {

  var len = file.length
  var line = 1
  var linePos = 1
  var ptr = 0
  val buffer = new StringBuilder

  def next:Token = {
    var t:Token = null

    while (t == null && len > 0) {
      val c = this.look

      c match {
        case '\n' => {this.newLine(); this.step()}
        case '\r' | '\t' | ' ' => this.step()
        case '/' => t = this.slash
        case '\"' => t = this.lexerString
        case '_' => t = this.identifier
        case _ => {
          if (c.isLower) t = this.identifier
          else if (c.isDigit) t = this.number
          else if (c.isUpper) t = this.typeID
          else if (Lexer.isSymbol(c)) t = this.symbol
          else {
            pushError("Unrecognized character " + c)
            step()
          }
        }
      }
    }

    if (t == null) this.newToken(TK_EOF)
    else t
  }

  private def look: Char = file.charAt(ptr)

  private def step() {
    adv(1)
  }

  private def adv(count: Int) {
    assert(len <= count)

    ptr += count
    len -= count
    linePos += count
  }

  private def newLine() {
    line += 1
    linePos = 0
  }

  private def slash: Token = {
    this.step()

    if (len > 0) {
      if (this.look == '*') {
        this.step()
        this.nestedComment()
        return null
      } else if (this.look == '/') {
        this.step()
        this.lineComment()
        return null
      }
    }

    this.newToken(TK_DIVIDE)
  }

  private def nestedComment() {
    var lineDepth = 1

    while (lineDepth > 0) {
      if (len <= 1) {
        pushError("Nested comment doesn't terminate")
        ptr += len
        len = 0
        return
      }

      if (look == '*') {
        step()
        if (this.look == '/') {
          lineDepth -= 1
        }
      } else if (look == '/') {
        step()
        if (this.look == '*') {
          lineDepth += 1
        }
      } else if (look == '\n') {
        newLine()
      }

      step()
    }
  }

  private def lineComment() {
    while (len > 0 && look != '\n')
      step()
  }

  private def lexerString: Token = {
    step()

    assert(buffer.size == 0)

    while (true) {
      if (len == 0) {
        stringTerminate()
        return null
      } else if (look == '\"') {
        step()
        return newToken(buffCopy(), TK_STRING)
      } else if (look == '\\') {
        if (len < 2) {
          stringTerminate()
          return null
        }

        step()
        val c = look
        step()

        c match {
          case 'b' => append('\b')
          case 'f' => append('\f')
          case 'n' => append('\n')
          case 'r' => append('\r')
          case 't' => append('\t')
          case '\"' => append('\"')
          case '\\' => append('\\')
          case '0' => append('\0')
          case _ => {pushError("Invalid escape sequence " + c)}
        }
      } else {
        append(look)
        step()
      }
    }

    throw new LexerException("Exited infinite loop, somehow")
  }

  private def stringTerminate() {
    pushError("Unterminated string")
    ptr += len
    len = 0
    buffer.clear()
  }

  def append(c: Char) {
    buffer += c
  }

  private def buffCopy(): String = {
    val str = buffer.toString()
    buffer.clear()
    str
  }

  private def identifier: Token = {

    readIdentifier()

    for (keyword <- Lexer.keywords) {
      if (buffer.toString() == keyword.symbol) {
        return newToken(keyword.id)
      }
    }

    newToken(buffCopy(), TK_ID)
  }

  private def readIdentifier() {
    while (len > 0) {
      val c = look

      if (c == '_' || c.isLetterOrDigit) {
        append(c)
        step()
      } else {
        return
      }
    }
  }

  private def number: Token = {

    if (look == '0') {
      step()

      if (len > 0) {

        look match {
          case 'x' => hexadecimal
          case 'b' => binary
          case   _ => decimal
        }
      }
    }

    this.decimal
  }

  private def decimal: Token = {
    var v = 0
    var error = false

    while (len > 0) {
      val c = look

      if (c.isDigit) {
        v = (v * 10) + c.toString.toInt
      } else if (c == '.' || c == 'e' || c == 'E') {
        return real(v)
      } else if (c == '_') { // skip this one
      } else if (c.isLetter) {
        if (!error) {
          pushError("Invalid digit in decimal number: " + c)
          error = true
        }
      } else {
        if (error) return null
        return newToken(v, TK_INT)
      }

      step()
    }

    throw new LexerException("Unreachable")
  }

  private def real(v: Int): Token = {
    var d: Double = v.toDouble
    var digits = -1
    var e = 0
    var error = false

    if (look == '.') {
      step()
      digits = 0
    }

    val breakOne = new Breaks
    import breakOne.{break, breakable}

    breakable {
      while (len > 0) {
        val c = look

        if (c.isDigit) {
          d = (d * 10) + c.toString.toInt
          digits += 1
          e -= 1
        } else if (c == 'E' || c == 'e') break()
        else if (c == '_') {}
        else if (c.isLetter) {
          if (!error) {
            pushError("Invalid digit in real number: " + c)
            error = true
          }
        } else break()

        step()
      }
    }

    if (digits == 0) {
      pushError("Real number has no digits following '.'")
      error = true
    }

    if (len > 0 && (look == 'e' || look == 'E')) {
      step()
      digits = 0

      if (len == 0) {
        pushError("Real number doesn't terminate")
        return null
      }

      val c = look
      var neg = c == '-'
      var n = 0

      if (c == '+' || c == '-') {
        step()

        if (len == 0) {
          pushError("Real number doesn't terminate")
          return null
        }
      }

      breakable {
        while (len > 0) {
          val c = look

          if (c.isDigit) {
            n = (n * 10) + c.toString.toInt
            digits += 1
          } else if (c == '_') {}
            else if (c.isLetter) {
              if (!error) {
                pushError("Invalid digit in real number: " + c)
                error = true
              }
          } else break()


          step()
        }
      }

      if (neg) e -= n
      else e += n

      if (digits == 0) {
        pushError("Exponent has no digits")
        error = true
      }
    }

    if (error) return null

    newToken(d * math.pow(10.0, e), TK_FLOAT)
  }

  private def hexadecimal: Token = {
    var v = 0
    var error = false

    val breakOne = new Breaks
    import breakOne.{break, breakable}

    breakable {
      while (len > 0) {
        val c: Char = look

        if (c.isDigit) {
          v = (v * 16) + c.toString.toInt
        } else if (c.isLower) {
          v = (v * 16) + (c.toInt - 'a'.toInt)
        } else if (c.isUpper) {
          v = (v* 16) + (c.toInt - 'A'.toInt)
        } else if (c == '_') {
        } else if (c.isLetter) {
          if (!error) {
            pushError("Invalid digit in hexadecimal number: " + c)
            error = true
          }
        } else break()

        step()
      }
    }

    if (error) null
    else newToken(v, TK_INT)
  }

  private def binary : Token = {
    var v = 0
    var error = false

    val breakOne = new Breaks
    import breakOne.{break, breakable}

    breakable {
      while (len > 0) {
        look match {
          case '0' => v *= 2
          case '1' => v = v*2 + 1
          case '_' => {}
          case   _ => {
            if (look.isLetter) {
              if (!error) {
                pushError("Invalid digit in decimal number: " + look)
                error = true
              } else break()
            }
          }
        }

        step()
      }
    }

    if (error) null
    else newToken(v, TK_INT)
  }

  private def typeID: Token = {
    readIdentifier()

    newToken(buffCopy(), TK_TYPEID)
  }

  private def symbol: Token = {
    val sym = Array.ofDim[Char](2)

    sym(0) = look
    step()

    if (len > 1) {
      sym(1) = look
      for (p <- Lexer.symbols2) {
        if (sym(0) == p.symbol(0) && sym(1) == p.symbol(1)) {
          step()
          return newToken(p.id)
        }
      }
    }

    for (p <- Lexer.symbols1) {
      if (sym(0) == p.symbol(0)) {
        return newToken(p.id)
      }
    }

    pushError("Unknown symbol: " + sym(0))
    null
  }

  private def newToken(tok: TokenType): Token = new ProgramToken(tok, filename, line, linePos)
  private def newToken(string: String, tokType: TokenType): Token = new StringToken(string, tokType, filename, line, linePos)
  private def newToken(num: Int, tokType: TokenType): Token = new IntToken(num, tokType, filename, line, linePos)
  private def newToken(num: Double, tokType: TokenType): Token = new DoubleToken(num, tokType, filename, line, linePos)

  private def pushError(msg:String) { errorList += newError(msg) }

  private def newError(msg: String): CompilerError = new CompilerError(this.filename, this.line, this.linePos, msg)

}

object Lexer {

  def isSymbol(c: Char): Boolean = ((c >= '!') && (c <= '.')) || ((c >= ':') && (c <= '@')) || ((c >= '[') && (c <= '^')) || ((c >= '{') && (c <= '~'))

  val symbols1: Seq[Symbol_T] = Seq(
    new Symbol_T("->", TK_RESULTS ),
    new Symbol_T( "::", TK_PACKAGE ),

    new Symbol_T( "<<", TK_LSHIFT ),
    new Symbol_T( ">>", TK_RSHIFT ),

    new Symbol_T( "==", TK_EQ ),
    new Symbol_T( "!=", TK_NOTEQ ),
    new Symbol_T( "#=", TK_STEQ ),
    new Symbol_T( "~=", TK_NSTEQ ),

    new Symbol_T( "[:", TK_MODE ),

    new Symbol_T( "<=", TK_LE ),
    new Symbol_T( ">=", TK_GE )
  )

  val symbols2: Seq[Symbol_T] = Seq(
    new Symbol_T("{", TK_LBRACE),
    new Symbol_T("}", TK_RBRACE),
    new Symbol_T("(", TK_LPAREN),
    new Symbol_T(")", TK_RPAREN),
    new Symbol_T("[", TK_LBRACKET),
    new Symbol_T("]", TK_RBRACKET),
    new Symbol_T(",", TK_COMMA),

    new Symbol_T(".", TK_CALL),
    new Symbol_T(":", TK_OFTYPE),
    new Symbol_T("\\", TK_PARTIAL),
    new Symbol_T("=", TK_ASSIGN),
    new Symbol_T("!", TK_BANG),

    new Symbol_T("+", TK_PLUS),
    new Symbol_T("-", TK_MINUS),
    new Symbol_T("*", TK_MULTIPLY),
    new Symbol_T("/", TK_DIVIDE),
    new Symbol_T("%", TK_MOD),

    new Symbol_T("<", TK_LT),
    new Symbol_T(">", TK_GT),

    new Symbol_T("|", TK_OR),
    new Symbol_T("&", TK_AND),
    new Symbol_T("^", TK_XOR),

    new Symbol_T("@", TK_UNIQ),
    new Symbol_T("~", TK_MUT),

    new Symbol_T(";", TK_SCOLON)
  )

  val keywords: Seq[Symbol_T] = Seq(
    new Symbol_T("use", TK_USE),
    new Symbol_T("declare", TK_DECLARE),
    new Symbol_T("type", TK_TYPE),
    new Symbol_T("lambda", TK_LAMBDA),
    new Symbol_T("trait", TK_TRAIT),
    new Symbol_T("object", TK_OBJECT),
    new Symbol_T("actor", TK_ACTOR),
    new Symbol_T("is", TK_IS),
    new Symbol_T("var", TK_VAR),
    new Symbol_T("delegate", TK_DELEGATE),
    new Symbol_T("new", TK_NEW),
    new Symbol_T("ambient", TK_AMBIENT),
    new Symbol_T("function", TK_FUNCTION),
    new Symbol_T("message", TK_MESSAGE),
    new Symbol_T("throws", TK_THROWS),
    new Symbol_T("throw", TK_THROW),
    new Symbol_T("return", TK_RETURN),
    new Symbol_T("break", TK_BREAK),
    new Symbol_T("continue", TK_CONTINUE),
    new Symbol_T("if", TK_IF),
    new Symbol_T("else", TK_ELSE),
    new Symbol_T("for", TK_FOR),
    new Symbol_T("in", TK_IN),
    new Symbol_T("while", TK_WHILE),
    new Symbol_T("do", TK_DO),
    new Symbol_T("match", TK_MATCH),
    new Symbol_T("case", TK_CASE),
    new Symbol_T("as", TK_AS),
    new Symbol_T("catch", TK_CATCH),
    new Symbol_T("always", TK_ALWAYS),
    new Symbol_T("this", TK_THIS),
    new Symbol_T("true", TK_TRUE),
    new Symbol_T("false", TK_FALSE)
  )
}