import lexer.Identifier
import parser._

import scala.collection.immutable.ListMap

object evaluator {
  abstract class Value
  case object Null extends Value
  case class Num(value: Double) extends Value
  case class Bool(value: Boolean) extends Value
  case class ListValue[T <: Value](vals: List[T]) extends Value
  case class FuncValue(func: FuncDecl) extends Value

  def performArithmetic(d: Double, d1: Double, operation: parser.BinaryOperation): Value = {
    operation match {
      case Plus => Num(d + d1)
      case Minus => Num(d - d1)
      case Times => Num(d * d1)
      case Div => Num(d / d1)
      case DoubleEquals => Bool(d == d1)
      case LessThanEquals => Bool(d <= d1)
      case LessThan => Bool(d < d1)
      case GreaterThanEquals => Bool(d >= d1)
      case GreaterThan => Bool(d > d1)
    }
  }

  def performBool(l: Boolean, r: Boolean, op: parser.BinaryOperation): Value = {
    op match {
      case DoubleEquals => Bool(l == r)
    }
  }

  def applyFunction(func: parser.StatementNode, vals: List[Value], varBindings: ListMap[String, evaluator.Value]): Value = {
    evaluateStatement(func, varBindings) match {
      case FuncValue(f) => {
        vals match {
          case Nil => ListValue(List())
          case x :: xs => applyFunction(func, xs, varBindings) match {
            case ListValue(t) =>
              ListValue(
                evaluateFunctionWithArguments(f, varBindings + (f.variables.head.name -> x)) :: t)
          }
        }
      }
    }
  }

  def filterFunction(func: parser.StatementNode, vals: List[Value], varBindings: ListMap[String, evaluator.Value]): Value = {
    evaluateStatement(func, varBindings) match {
      case FuncValue(f) => {
        vals match {
          case Nil => ListValue(List())
          case x :: xs => filterFunction(func, xs, varBindings) match {
            case ListValue(t) =>
                evaluateFunctionWithArguments(f, varBindings + (f.variables.head.name -> x)) match {
                  case Bool(true) => {
                    ListValue(x::t)
                  }
                  case Bool(false) => {
                    ListValue(t)
                  }
                }
          }
        }
      }
    }
  }

  def evaluateStatement(stmnt: StatementNode, varBindings: ListMap[String, Value]): Value = stmnt match {
    case IdentifierStatementNode(name) => varBindings.get(name) match {
      case None => sys.error("No identifier " + name)
      case Some(value) => value
    }
    case NumberStatementNode(value) => Num(value)
    case BooleanStatementNode(value) => Bool(value)
    case BinaryOperationNode(left, right, op) => (evaluateStatement(left, varBindings), evaluateStatement(right, varBindings)) match {
      case (Num(l), Num(r)) => performArithmetic(l, r, op)
      case (Bool(l), Bool(r)) => performBool(l, r, op)
      case _ => sys.error("Invalid args to binary Operation")
    }
    case MapOperation(funcName, lstName) => {
      evaluateStatement(lstName, varBindings) match {
        case ListValue(vals) => applyFunction(funcName, vals, varBindings)
      }
    }
    case FilterOperation(funcName, lstName) => {
      evaluateStatement(lstName, varBindings) match {
        case ListValue(vals) => filterFunction(funcName, vals, varBindings)
      }
    }
  }

  def evaluateFunctionWithArguments(function: Decl, args: ListMap[String, Value]): Value = {
    function match {
      case t @ FuncDecl(_, _, _, _) => evaluateStatement(t.body, args)
    }
  }

  def evaluateFunction(function: Decl, args: List[Value], existing: ListMap[String, Value]): Value = {
    function match {
      case t @ FuncDecl(_, _, vars, body) => {
        var passArgs = ListMap[String, Value]()
        var i = 0

        for (variable <- vars) {
          variable match {
            case Identifier(value) => {
              passArgs += (value -> args(i))
              i += 1
            }
          }
        }

        passArgs = passArgs ++ existing

        return evaluateStatement(body, passArgs)
      }
    }
    null
  }
}
