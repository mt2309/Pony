package com.github.mt2309.pony.AST

/**
 * User: mthorpe
 * Date: 26/04/2013
 * Time: 00:16
 */
sealed abstract class TokenType

case object TK_STRING extends TokenType
case object TK_INT extends TokenType
case object TK_FLOAT extends TokenType
case object TK_ID extends TokenType
case object TK_TYPEID extends TokenType

// symbols
case object TK_LBRACE extends TokenType
case object TK_RBRACE extends TokenType
case object TK_LPAREN extends TokenType
case object TK_RPAREN extends TokenType
case object TK_LBRACKET extends TokenType
case object TK_RBRACKET extends TokenType
case object TK_COMMA extends TokenType
case object TK_RESULTS extends TokenType

case object TK_CALL extends TokenType
case object TK_PACKAGE extends TokenType
case object TK_OFTYPE extends TokenType
case object TK_PARTIAL extends TokenType
case object TK_ASSIGN extends TokenType
case object TK_BANG extends TokenType

case object TK_PLUS extends TokenType
case object TK_MINUS extends TokenType
case object TK_MULTIPLY extends TokenType
case object TK_DIVIDE extends TokenType
case object TK_MOD extends TokenType

case object TK_LSHIFT extends TokenType
case object TK_RSHIFT extends TokenType

case object TK_LT extends TokenType
case object TK_LE extends TokenType
case object TK_GE extends TokenType
case object TK_GT extends TokenType

case object TK_EQ extends TokenType
case object TK_NOTEQ extends TokenType
case object TK_STEQ extends TokenType
case object TK_NSTEQ extends TokenType

case object TK_OR extends TokenType
case object TK_AND extends TokenType
case object TK_XOR extends TokenType

case object TK_UNIQ extends TokenType
case object TK_MUT extends TokenType
case object TK_MODE extends TokenType

// keywords
case object TK_USE extends TokenType
case object TK_DECLARE extends TokenType
case object TK_TYPE extends TokenType
case object TK_LAMBDA extends TokenType
case object TK_TRAIT extends TokenType
case object TK_OBJECT extends TokenType
case object TK_ACTOR extends TokenType
case object TK_IS extends TokenType
case object TK_VAR extends TokenType
case object TK_DELEGATE extends TokenType
case object TK_NEW extends TokenType
case object TK_AMBIENT extends TokenType
case object TK_FUNCTION extends TokenType
case object TK_MESSAGE extends TokenType
case object TK_THROWS extends TokenType
case object TK_THROW extends TokenType
case object TK_RETURN extends TokenType
case object TK_BREAK extends TokenType
case object TK_CONTINUE extends TokenType
case object TK_IF extends TokenType
case object TK_ELSE extends TokenType
case object TK_FOR extends TokenType
case object TK_IN extends TokenType
case object TK_WHILE extends TokenType
case object TK_DO extends TokenType
case object TK_MATCH extends TokenType
case object TK_CASE extends TokenType
case object TK_AS extends TokenType
case object TK_CATCH extends TokenType
case object TK_ALWAYS extends TokenType
case object TK_THIS extends TokenType
case object TK_TRUE extends TokenType
case object TK_FALSE extends TokenType

// abstract
case object TK_MODULE extends TokenType
case object TK_DECLAREMAP extends TokenType
case object TK_MAP extends TokenType
case object TK_TYPEBODY extends TokenType
case object TK_TYPECLASS extends TokenType
case object TK_FORMALARGS extends TokenType
case object TK_FIELD extends TokenType
case object TK_ARG extends TokenType
case object TK_ARGS extends TokenType
case object TK_BLOCK extends TokenType
case object TK_CASEVAR extends TokenType
case object TK_LIST extends TokenType

case object TK_SCOLON extends TokenType

case object TK_EOF extends TokenType

