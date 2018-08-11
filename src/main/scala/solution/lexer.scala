
package solution


import scala.util.parsing.combinator.RegexParsers

object lexer {
  abstract class Token

  case class Number(value: Double) extends Token
  case class Bool(value: Boolean) extends Token
  case class Identifier(name: String) extends Token
  case class Typename(name: String) extends Token
  case class BinaryOp(value: String) extends Token

  case object Arrow extends Token
  case object Colons extends Token
  case object Equals extends Token
  case object LeftSqBracket extends Token
  case object RightSqBracket extends Token
  case object DrawnFrom extends Token
  case object MapT extends Token
  case object LambdaBegin extends Token
  case object LeftBracket extends Token
  case object RightBracket extends Token
  case object Colon extends Token

  case object FilterToken extends Token


  object HaskellLexer extends RegexParsers {

    override def skipWhitespace = true
    override val whiteSpace = "[ \t\r\f]+".r

    def identifier: Parser[Identifier] = {
      "[a-z][a-zA-Z0-9_]*".r ^^ { str => Identifier(str) }
    }

    def trueToken: Parser[Bool] = {
      "True" ^^ {_ => Bool(true)}
    }

    def falseToken: Parser[Bool] = {
      "False" ^^ {_ => Bool(false)}
    }

    def boolean: Parser[Bool] = trueToken | falseToken

    def number: Parser[Number] = {
      "[-+]?[0-9]*\\.?[0-9]+".r ^^ {str => Number(str.toDouble)}
    }

    def typename: Parser[Typename] = {
      "[A-Z][a-zA-Z0-9]*".r ^^ {str => Typename(str)}
    }

    def typearrow: Parser[Arrow.type] = "->" ^^ (_ => Arrow)

    def colons: Parser[Colons.type] = "::" ^^ (_ => Colons)

    def equals: Parser[Equals.type ] = "=" ^^ (_ => Equals)

    def binop: Parser[BinaryOp] = "\\+|\\-|\\*|/|==|>=|<=|<|>".r ^^ {str => BinaryOp(str)}

    def leftsqbracket: Parser[LeftSqBracket.type] = "[" ^^ (_ => LeftSqBracket)

    def rightsqbracket: Parser[RightSqBracket.type] = "]" ^^ (_ => RightSqBracket)

    def mapT: Parser[MapT.type] = "map" ^^ (_ => MapT)

    def drawnfrom: Parser[DrawnFrom.type] = "<-" ^^ (_ => DrawnFrom)


    def filterT: Parser[FilterToken.type ] = "filter" ^^ (_ => FilterToken)


    def tokens: Parser[List[Token]] = {
      phrase(rep1(
          boolean |
          number |
          typename |
          typearrow |
          colons |
            binop |
            equals |
          leftsqbracket |
          rightsqbracket |
            drawnfrom |
            mapT |
            filterT |
            identifier
      )) ^^ {rawTokens => rawTokens}
    }

    def apply(code: String): Either[HaskellCompilationError, List[Token]] = {
      parse(tokens, code) match {
        case NoSuccess(msg, next) => Left(LexerError(msg))
        case Success(result, next) => Right(result)
      }
    }
  }
}
