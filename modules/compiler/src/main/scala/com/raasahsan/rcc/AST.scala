package com.raasahsan.rcc

import cats.syntax.all._
import cats.data.NonEmptyList

object AST {
  // The unit of program text after preprocessing is called a translation unit,
  // which consists of a sequence of external declarations.
  final case class TranslationUnit(externalDeclarations: NonEmptyList[ExternalDeclaration])

  enum ExternalDeclaration {
    case FunctionDefinition(value: AST.FunctionDefinition)
    case Declaration(value: AST.Declaration)
  }

  final case class FunctionDefinition(
      specifiers: Option[DeclarationSpecifiers],
      declarator: Declarator,
      declarationList: Option[DeclarationList],
      statements: CompoundStatement
  ) extends Typable {
    def functionName: Option[Identifier] = declarator.functionName
    def functionParameters: Option[List[(Identifier, DeclarationSpecifiers)]] =
      declarator.functionParameters
  }

  final case class DeclarationList(declarations: NonEmptyList[Declaration])

  final case class StatementList(statements: NonEmptyList[Statement])

  enum Statement {
    case Labeled(stmt: LabeledStatement)
    case Compound(stmt: CompoundStatement)
    case Expression(stmt: ExpressionStatement)
    case Selection(stmt: SelectionStatement)
    case Iteration()
    case Jump(stmt: JumpStatement)
  }

  final case class ExpressionStatement(expr: Option[Expression])

  final case class LabeledStatement()

  enum SelectionStatement {
    case If(condition: Expression, consequent: Statement, alternative: Option[Statement])
  }

  final case class CompoundStatement(
      declarationList: Option[DeclarationList],
      statementList: Option[StatementList]
  )

  // Unconditional jump to another location in the program
  enum JumpStatement {
    case Goto(id: Identifier)
    case Continue
    case Break
    case Return(expression: Option[Expression])
  }

  final case class Declaration(
      specifiers: DeclarationSpecifiers,
      initDeclaratorList: Option[InitDeclaratorList]
  )

  final case class DeclarationSpecifiers(specifiers: NonEmptyList[DeclarationSpecifier])

  enum DeclarationSpecifier {
    case StorageClassSpecifier(value: AST.StorageClassSpecifier)
    case TypeSpecifier(value: AST.TypeSpecifier)
    case TypeQualifier(value: AST.TypeQualifier)
  }

  enum StorageClassSpecifier {
    case Typedef
    case Extern
    case Static
    case Auto
    case Register
  }

  enum TypeSpecifier {
    case Void
    case Char
    case Short
    case Int
    case Long
    case Float
    case Double
    case Signed
    case Unsigned
    case StructOrUnion()
    case Enum()
    case TypedefName()
  }

  final case class TypeQualifierList(qualifiers: NonEmptyList[TypeQualifier])

  enum TypeQualifier {
    case Const
    case Static
  }

  final case class InitDeclaratorList(declarators: NonEmptyList[InitDeclarator])

  final case class InitDeclarator(declarator: Declarator, initializer: Option[Initializer])

  final case class Declarator(pointer: Option[Pointer], directDeclarator: DirectDeclarator) {
    def identifier: Option[Identifier] = directDeclarator.identifier
    def functionName: Option[Identifier] = directDeclarator.functionName
    def functionParameters: Option[List[(Identifier, DeclarationSpecifiers)]] =
      directDeclarator.functionParameters
  }

  final case class ParameterTypeList(parameterList: ParameterList, repeated: Boolean)

  final case class ParameterList(parameters: NonEmptyList[ParameterDeclaration])

  final case class IdentifierList(identifiers: NonEmptyList[Identifier])

  // TODO: abstract declarator
  enum ParameterDeclaration {
    case Declarator(specifiers: DeclarationSpecifiers, declarator: AST.Declarator)
  }

  final case class Pointer(typeQualifiers: NonEmptyList[Option[TypeQualifierList]])

  enum DirectDeclarator {
    case Identifier(value: AST.Identifier)
    case Declarator(value: AST.Declarator)
    case FunctionDeclarator(decl: DirectDeclarator, parameterTypeList: ParameterTypeList)
    case Identifiers(decl: DirectDeclarator, identifiers: Option[IdentifierList])

    def identifier: Option[AST.Identifier] =
      this match {
        case DirectDeclarator.Identifier(i) => Some(i)
        case _                              => None
      }

    // TODO: This can probably be merged with functionParameters
    def functionName: Option[AST.Identifier] =
      this match {
        case DirectDeclarator.FunctionDeclarator(decl, _) =>
          decl.identifier
        case DirectDeclarator.Identifiers(decl, _) =>
          decl.identifier
        case _ => None
      }

    def functionParameters: Option[List[(AST.Identifier, DeclarationSpecifiers)]] =
      this match {
        case FunctionDeclarator(_, params) =>
          params.parameterList.parameters.toList.map {
            case ParameterDeclaration.Declarator(specs, decl) =>
              decl.identifier.get -> specs
          }.some
        case Identifiers(_, None) => Some(Nil)
        case _                    => None
      }
  }

  final case class Identifier(value: String)

  enum Initializer {
    case Expression(expression: AST.Expression)
    case Initializers(initializers: InitializerList)
  }

  final case class InitializerList(initializers: NonEmptyList[Initializer])

  final case class ArgumentExpressionList(args: NonEmptyList[Expression])

  // TODO: eliminate this mutability in the future
  // limitations: impure, we may forget to attribute a type
  // ill-typed tree may yield a partially typed tree
  // ideas:
  // 1. duplicate tree AST with explicit typed expressions (guarantees type by construction)
  // 2. Cofree for metadata
  // 3. F[_] for metadata
  trait Typable {
    var tpe: Option[Type] = None
  }

  sealed trait Expression extends Typable

  object Expression {
    final case class Constant(constant: AST.Constant) extends Expression
    final case class Identifier(identifier: AST.Identifier) extends Expression
    final case class StringLiteral(literal: AST.StringLiteral) extends Expression
    final case class Assignment(lhs: Expression, rhs: Expression) extends Expression
    final case class Plus(lhs: Expression, rhs: Expression) extends Expression
    final case class Minus(lhs: Expression, rhs: Expression) extends Expression
    final case class Times(lhs: Expression, rhs: Expression) extends Expression
    final case class Divide(lhs: Expression, rhs: Expression) extends Expression
    final case class Modulo(lhs: Expression, rhs: Expression) extends Expression
    final case class FunctionCall(lhs: Expression, args: Option[ArgumentExpressionList])
        extends Expression
    final case class ArrayGet(lhs: Expression, index: Expression) extends Expression
    final case class Dereference(op: Expression) extends Expression
  }

  enum AssignmentOperator {
    case Assign
    case StarAssign
    case DivAssign
    case ModAssign
    case PlusAssign
    case MinusAssign
    case ShlAssign
    case ShrAssign
    case AndAssign
    case XorAssign
    case OrAssign
  }

  enum Constant {
    case IntegerConstant(value: Int)
  }

  final case class StringLiteral(value: String)

}
