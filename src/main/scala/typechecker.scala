import javafx.beans.property.ReadOnlyDoubleWrapper
import parser._
import lexer._
import scala.collection.immutable.ListMap

object typechecker {

  abstract class TypeNode
  case class MultiType(first: UnaryType, rest: TypeNode) extends TypeNode
  abstract class UnaryType extends TypeNode
  case class ListType(elemType: TypeNode) extends UnaryType
  abstract class SimpleType extends UnaryType
  case object IntType extends SimpleType
  case object BoolType extends SimpleType

  def sameType(left: TypeNode, right: TypeNode): Boolean = (left, right) match {
    case (MultiType(fst, rest), MultiType(fst2, rest2)) => {
      sameType(fst, fst2) && sameType(rest, rest2)
    }
    case (ListType(elem), ListType(elem2)) => sameType(elem, elem2)
    case (IntType, IntType) => true
    case (BoolType, BoolType) => true
    case _ => false
  }

  def getTypeForStatement(statement: StatementNode, typeEnvironment: ListMap[String, TypeNode]): TypeNode =
    statement match {
      case NumberStatementNode(_) => IntType
      case BooleanStatementNode(_) => BoolType
      case IdentifierStatementNode(name) => typeEnvironment.get(name) match {
        case None => sys.error("Could not find type of variable")
        case Some(value) => value
      }
      case BinaryOperationNode(leftOperand, rightOperand, operation) => {
        (getTypeForStatement(leftOperand, typeEnvironment), getTypeForStatement(rightOperand, typeEnvironment)) match {
          case (IntType, IntType) => operation match {
            case Minus|Plus|Times|Div => IntType
            case _ => BoolType
          }
          case (BoolType, BoolType) => operation match {
            case DoubleEquals => BoolType
            case _ => sys.error("Could not perform binary operation on bool types")
          }
        }
      }
      case MapOperation(fName, listName) => (fName, listName) match {
        case (IdentifierStatementNode(name), IdentifierStatementNode(list)) => typeEnvironment.get(name) match {
          case None => sys.error("Could not find type of function in map")
          case Some(typ) => typ match {
            case MultiType(fst, rest) => {
              typeEnvironment.get(list) match {
                case None => sys.error("Could not find type of list in map")
                case Some(ListType(node)) => {
                  if (!sameType(node, fst)) {
                    sys.error("Trying to apply wrong function")
                  }
                  ListType(rest)
                }
              }
            }
          }
        }
      }
      case FilterOperation(fName, listName) => (fName, listName) match {
        case (IdentifierStatementNode(name), IdentifierStatementNode(list)) => typeEnvironment.get(name) match {
          case None => sys.error("Could not find type of function in map")
          case Some(typ) => typ match {
            case MultiType(fst, rest) => {
              typeEnvironment.get(list) match {
                case None => sys.error("Could not find type of list in map")
                case Some(ListType(node)) => {
                  if (!sameType(node, fst)) {
                    sys.error("Trying to apply wrong function")
                  }
                  rest match {
                    case BoolType => ListType(node)
                  }
                }
              }
            }
          }
        }
      }
  }

  def typeCheckFunction(function: Decl, typeMap: ListMap[String, TypeNode]): Boolean = function match {
    case FuncDecl(name, typeSignature, variables, body) => {
      var typeSig = typeSignature
      var mutable_typeMap = typeMap
      for (variable <- variables) {
        if (typeSig == null) {
          sys.error("More variables than are present in type signature")
        }
        typeSig match {
          case MultiType(lft, right) => {
            mutable_typeMap = mutable_typeMap + (variable.name -> lft)
            typeSig = right
          }
          case t => {
            mutable_typeMap = mutable_typeMap + (variable.name -> t)
            typeSig = null
          }
        }
      }
      if (typeSig == null) {
        sys.error("No return type present")
      }
      sameType(
        getTypeForStatement(body, mutable_typeMap),
        typeSig
      )
    }
  }


}
