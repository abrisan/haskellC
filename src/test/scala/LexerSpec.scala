import lexer._
import org.scalatest._

class LexerSpec extends FlatSpec {
  "The lexer" should "correctly lex simple strings" in {
    val testStrings: List[String] = List(
      "identifier",
      "True",
      "False",
      "123",
      "-123",
      "1.23",
      "1.023",
      "1.230",
      "->",
      "::",
      "=",
      "+",
      "-",
      "/",
      "*",
      ">",
      ">=",
      "<=",
      "==",
      "<",
      "[",
      "]",
      "map",
      "filter",
      "<-"
    )

    val expectedValues: List[Token] = List(
      Identifier("identifier"),
      Bool(true),
      Bool(false),
      Number(123),
      Number(-123),
      Number(1.23),
      Number(1.023),
      Number(1.23),
      Arrow,
      Colons,
      Equals,
      BinaryOp("="),
      BinaryOp("+"),
      BinaryOp("-"),
      BinaryOp("/"),
      BinaryOp("*"),
      BinaryOp(">"),
      BinaryOp(">="),
      BinaryOp("<="),
      BinaryOp("=="),
      BinaryOp("<"),
      LeftSqBracket,
      RightSqBracket,
      MapT,
      FilterT,
      DrawnFrom
    )

    val actualTokens: List[Token] = testStrings.map(x => HaskellLexer(x) match {
      case Left(_) => Identifier("error")
      case Right(tokens) => tokens.head
    })

    for ((actual, expected) <- actualTokens.zip(expectedValues)) {
      assert(actual == expected)
    }
  }
}
