package com.raasahsan.rcc

import cats.syntax.all._
import cats.data.NonEmptyList

object TypedAST {

  final case class Cofree[F[_], A](a: A, next: F[Cofree[F, A]])
  type Expr[A] = Cofree[Expression, A]
  type RawExpr = Expr[Unit]
  type TypedExpr = Expr[Type]

  // The unit of program text after preprocessing is called a translation unit,
  // which consists of a sequence of external declarations.
  final case class TranslationUnit[A](externalDeclarations: NonEmptyList[ExternalDeclaration[A]])

  enum ExternalDeclaration[A] {
    case FunctionDefinition(value: TypedAST.FunctionDefinition[A])
    case Declaration(value: TypedAST.Declaration[A])
  }

  final case class FunctionDefinition[A](
      specifiers: Option[DeclarationSpecifiers],
      declarator: Declarator,
      declarationList: Option[DeclarationList[A]],
      statements: CompoundStatement[A]
  ) {
    def functionName: Option[Identifier] = declarator.functionName
  }

  final case class DeclarationList[A](declarations: NonEmptyList[Declaration[A]])

  final case class StatementList[A](statements: NonEmptyList[Statement[A]])

  enum Statement[A] {
    case Labeled(stmt: LabeledStatement)
    case Compound(stmt: CompoundStatement[A])
    case Expression(stmt: ExpressionStatement[A])
    case Selection(stmt: SelectionStatement[A])
    case Iteration()
    case Jump(stmt: JumpStatement[A])
  }

  final case class ExpressionStatement[A](expr: Option[Expr[A]])

  final case class LabeledStatement()

  enum SelectionStatement[A] {
    case If(condition: A, consequent: Statement[A], alternative: Option[A])
  }

  final case class CompoundStatement[A](
      declarationList: Option[DeclarationList[A]],
      statementList: Option[StatementList[A]]
  )

  // Unconditional jump to another location in the program
  enum JumpStatement[A] {
    case Goto(id: Identifier)
    case Continue()
    case Break()
    case Return(expression: Option[Expr[A]])
  }

  final case class Declaration[A](
      specifiers: DeclarationSpecifiers,
      initDeclaratorList: Option[InitDeclaratorList[A]]
  )

  final case class DeclarationSpecifiers(specifiers: NonEmptyList[DeclarationSpecifier])

  enum DeclarationSpecifier {
    case StorageClassSpecifier(value: TypedAST.StorageClassSpecifier)
    case TypeSpecifier(value: TypedAST.TypeSpecifier)
    case TypeQualifier(value: TypedAST.TypeQualifier)
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

  final case class InitDeclaratorList[A](declarators: NonEmptyList[InitDeclarator[A]])

  final case class InitDeclarator[A](declarator: Declarator, initializer: Option[Initializer[A]])

  final case class Declarator(pointer: Option[Pointer], directDeclarator: DirectDeclarator) {
    def identifier: Option[Identifier] = directDeclarator.identifier
    def functionName: Option[Identifier] = directDeclarator.functionName
    def functionParameters: Option[List[Identifier]] = directDeclarator.functionParameters
  }

  final case class ParameterTypeList(parameterList: ParameterList, repeated: Boolean)

  final case class ParameterList(parameters: NonEmptyList[ParameterDeclaration])

  final case class IdentifierList(identifiers: NonEmptyList[Identifier])

  // TODO: abstract declarator
  enum ParameterDeclaration {
    case Declarator(specifiers: DeclarationSpecifiers, declarator: TypedAST.Declarator)
  }

  final case class Pointer(typeQualifiers: NonEmptyList[Option[TypeQualifierList]])

  enum DirectDeclarator {
    case Identifier(value: TypedAST.Identifier)
    case Declarator(value: TypedAST.Declarator)
    case FunctionDeclarator(decl: DirectDeclarator, parameterTypeList: ParameterTypeList)
    case Identifiers(decl: DirectDeclarator, identifiers: Option[IdentifierList])

    def identifier: Option[TypedAST.Identifier] =
      this match {
        case DirectDeclarator.Identifier(i) => Some(i)
        case _                              => None
      }

    // TODO: This can probably be merged with functionParameters
    def functionName: Option[TypedAST.Identifier] =
      this match {
        case DirectDeclarator.FunctionDeclarator(decl, _) =>
          decl.identifier
        case DirectDeclarator.Identifiers(decl, _) =>
          decl.identifier
        case _ => None
      }

    def functionParameters: Option[List[TypedAST.Identifier]] =
      this match {
        case FunctionDeclarator(_, params) =>
          params.parameterList.parameters.toList.map {
            case ParameterDeclaration.Declarator(_, decl) =>
              decl.identifier.get
          }.some
        case Identifiers(_, None) => Some(Nil)
        case _                    => None
      }
  }

  final case class Identifier(value: String)

  enum Initializer[A] {
    case Expression(expression: TypedAST.Expr[A])
    case Initializers(initializers: InitializerList[A])
  }

  final case class InitializerList[A](initializers: NonEmptyList[Initializer[A]])

  final case class ArgumentExpressionList[A](args: NonEmptyList[Expr[A]])

  enum Expression[A] {
    case Constant(constant: TypedAST.Constant)
    case Identifier(identifier: TypedAST.Identifier)
    case StringLiteral(literal: TypedAST.StringLiteral)
    case Assignment(lhs: A, rhs: A)
    case Plus(lhs: A, rhs: A)
    case Minus(lhs: A, rhs: A)
    case Times(lhs: A, rhs: A)
    case Divide(lhs: A, rhs: A)
    case Modulo(lhs: A, rhs: A)
    case FunctionCall(lhs: A, args: Option[ArgumentExpressionList[A]])
    case ArrayGet(lhs: A, index: A)
    case Dereference(op: A)
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
