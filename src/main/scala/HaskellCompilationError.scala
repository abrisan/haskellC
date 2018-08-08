abstract class HaskellCompilationError

case class LexerError(message: String) extends HaskellCompilationError
case class ParserError(message: String) extends HaskellCompilationError