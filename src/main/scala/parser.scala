import lexer._
import parser.HaskellParser
import typechecker._

import scala.collection.immutable.ListMap
import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.util.parsing.input.{NoPosition, Position, Reader}

object parser {



  abstract class BinaryOperation

  case object Minus extends BinaryOperation

  case object Plus extends BinaryOperation

  case object Times extends BinaryOperation

  case object Div extends BinaryOperation

  case object DoubleEquals extends BinaryOperation
  case object LessThan extends BinaryOperation
  case object GreaterThan extends BinaryOperation
  case object GreaterThanEquals extends BinaryOperation
  case object LessThanEquals extends BinaryOperation

  abstract class StatementNode
  case class BinaryOperationNode(leftOperand: StatementNode,
                                 rightOperand: StatementNode,
                                 operation: BinaryOperation) extends StatementNode

  case class MapOperation(functionName: StatementNode, listName: StatementNode) extends StatementNode
  case class IdentifierStatementNode(identName: String) extends StatementNode
  case class NumberStatementNode(value: Double) extends StatementNode
  case class BooleanStatementNode(value: Boolean) extends StatementNode


  abstract class Decl

  case class FuncDecl(name: String, typeSignature: TypeNode, variables: List[Identifier], body: StatementNode) extends Decl

  case class ErrorDecl(errorMsg: String) extends Decl

  case class Script(functions: List[Decl]) extends Decl

  class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
    override def first: Token = tokens.head

    override def atEnd: Boolean = tokens.isEmpty

    override def pos: Position = NoPosition

    override def rest: Reader[Token] = new TokenReader(tokens.tail)
  }

  object HaskellParser extends Parsers {
    override type Elem = Token

    def typename: Parser[Typename] = accept("typename", { case tn@Typename(ident) => tn })

    def binop: Parser[BinaryOp] = accept("binary op", { case bn@BinaryOp(name) => bn })

    def ident: Parser[Identifier] = accept("ident", { case id@Identifier(name) => id })

    def number: Parser[Number] = accept("number", { case num@Number(value) => num })

    def boolean: Parser[Bool] = accept("bool", { case b@Bool(value) => b })

    def identStmnt: Parser[IdentifierStatementNode] = ident ^^ { case Identifier(value) => IdentifierStatementNode(value) }

    def numberStmnt: Parser[NumberStatementNode] = number ^^ { case Number(value) => NumberStatementNode(value) }

    def booleanStmnt: Parser[BooleanStatementNode] = boolean ^^ { case Bool(value) => BooleanStatementNode(value) }

    def simpleType: Parser[SimpleType] = typename ^^ {
      case tn@Typename(name) => name match {
        case "Int" => IntType
        case "Bool" => BoolType
      }
    }

    def listType: Parser[ListType] = (LeftSqBracket ~ oneType ~ RightSqBracket) ^^ {
      case _ ~ types ~ _ => ListType(types)
    }

    def oneType: Parser[UnaryType] = simpleType | listType

    def binaryOperator: Parser[BinaryOperation] = binop ^^ {
      case bop@BinaryOp(name) => name match {
        case "+" => Plus
        case "-" => Minus
        case "/" => Div
        case "*" => Times
        case ">" => GreaterThan
        case "<" => LessThan
        case "==" => DoubleEquals
        case ">=" => GreaterThanEquals
        case "<=" => LessThanEquals
      }
    }

    def binaryOperation: Parser[BinaryOperationNode] = statementNodeNonBinary ~ binaryOperator ~ statementNode ^^ {
      case left ~ op ~ right => BinaryOperationNode(left, right, op)
    }

    def mapOperation: Parser[MapOperation] = MapT ~ statementNode ~ statementNode ^^ {
      case _ ~ name ~ lst => MapOperation(name, lst)
    }

    def statementNodeNonBinary: Parser[StatementNode] = identStmnt | numberStmnt | booleanStmnt | mapOperation

    def statementNode: Parser[StatementNode] = binaryOperation | statementNodeNonBinary

    def complexType: Parser[MultiType] = oneType ~ Arrow ~ types ^^ {
      case t1 ~ _ ~ ts => MultiType(t1, ts)
    }

    def types: Parser[TypeNode] = complexType | oneType

    def function: Parser[Decl] = ident ~ Colons ~ types ~ ident ~ rep1(ident) ~ Equals ~ statementNode ^^ {
      case (id1@Identifier(fName1)) ~ _ ~ typess ~ (id2@Identifier(fName2)) ~ names ~ _ ~ stmnt => {
        if (!fName1.equals(fName2)) {
          ErrorDecl("Function declaration does not match typedecl")
        }
        FuncDecl(fName1, typess, names, stmnt)
      }
    }

    def script: Parser[Script] = rep1(function) ^^ { case funcs => Script(funcs) }


    def apply(tokens: Seq[Token]): Either[ParserError, Decl] = {
      val reader = new TokenReader(tokens)
      script(reader) match {
        case NoSuccess(msg, next) => Left(ParserError(msg))
        case Success(result, next) => Right(result)
      }
    }


  }

  /*def main(args: Array[String]): Unit = {
    val code = "f :: Int -> Int f x = x + 1"
    HaskellLexer(code) match {
      case Left(msg) => println(msg)
      case Right(tokens) => {
        println(tokens)
        println(HaskellParser(tokens))
      }
    }
  }*/

}
