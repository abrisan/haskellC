import evaluator.{FuncValue, ListValue, Num, Value, evaluateFunction}
import lexer.{HaskellLexer, Identifier}
import parser.{Decl, FuncDecl, HaskellParser, Script}
import typechecker._

import scala.collection.immutable
import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer
import scala.io.{Source, StdIn}

object runner {

  def safePrint(s: String): Unit = {
    System.out.println(s)
  }

  def getFunctionHandle(name: String, funcs: List[Decl]): FuncDecl = {
    for (func <- funcs) {
      func match {
        case t @ FuncDecl(_, _, _, _) => {
          if (t.name.equals(name)) {
            return t
          }
        }
      }
    }
    sys.error("Could not find function")
  }

  def parseListValue(bigString: String): List[Value] = {
    val splitString = bigString.split(",")

    var buffer = ListBuffer[Value]()

    for (string <- splitString) {
      val no_spaces = string.trim

      buffer += Num(no_spaces.toDouble)
    }

    List[Value](ListValue(buffer.toList))
  }

  def parseIntoListValue(strings: Array[String]): List[Value] = {
    val bigString = strings.mkString(" ")

    if (bigString.charAt(0) == '[') {
      return parseListValue(bigString.substring(1, bigString.length-1))
    }

    var list = ListBuffer[Value]()

    for (string <- strings) {
      list += Num(string.toDouble)
    }

    list.toList
  }

  def main(args: Array[String]): Unit = {
    val fileContents = Source.fromFile(args(0))

    var code = new StringBuilder()

    for (line <- fileContents.getLines()) {
      code.append(" ")
      code.append(line)
    }

    fileContents.close()

    val simpleListValue: ListValue[Num] = ListValue(List(Num(1), Num(2), Num(3)))


    HaskellLexer(code.toString()) match {
      case Left(msg) => println(msg)
      case Right(lexed) => {
        HaskellParser(lexed) match {
          case Left(msg) => println(msg)
          case Right(tree) => tree match {
            case Script(nodes) => {
              var typeMap = ListMap[String, TypeNode]()
              var funcMap = ListMap[String, Value]()

              for (func <- nodes) {
                func match {
                  case t @ FuncDecl(_, typeSignature, variables, _) => {
                    typeMap = typeMap + (t.name -> typeSignature)
                    funcMap += (t.name -> FuncValue(t))
                  }
                }
              }

              for (func <- nodes) {
                func match {
                  case t @ FuncDecl(_, typeSignature, variables, _) => {
                    if (!typeCheckFunction(func, typeMap)) {
                      safePrint("$ Function " + t.name + " does not typecheck. Aborting interpreter")
                      sys.error("Quitting")
                    }
                  }
                }
              }

              println ("$ Loaded haskell script.")

              var cond = true

              while (cond) {
                System.out.print("$ ")
                val command = StdIn.readLine("")
                if (command.equals("quit")) {
                  return
                }
                val splitCommand = command.split("\\s+")

                val funcName = splitCommand(0)

                val funcHandle = getFunctionHandle(funcName, nodes)

                if (splitCommand.tail.length > 1) {
                  val fArgs = parseIntoListValue(splitCommand.tail)
                  safePrint(evaluateFunction(funcHandle, fArgs, funcMap).toString)
                }
                else {
                  val fArgs = List(Num(splitCommand(1).toDouble))
                  safePrint(evaluateFunction(funcHandle, fArgs, funcMap).toString)
                }
              }

            }
          }
        }
      }
    }
  }
}
