package com.raasahsan.rcc

import cats.syntax.all._
import cats.data.NonEmptyList
import cats.data.Ior

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
  ) {
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
      initDeclarators: Option[NonEmptyList[InitDeclarator]]
  )

  final case class DeclarationSpecifiers(specifiers: NonEmptyList[DeclarationSpecifier]) {
    def typeSpecifiers: List[TypeSpecifier] = 
      specifiers.collect {
        case DeclarationSpecifier.TypeSpecifier(ts) => ts
      }
    def typeQualifiers: List[TypeQualifier] = 
      specifiers.collect {
        case DeclarationSpecifier.TypeQualifier(ts) => ts
      }
  }

  enum DeclarationSpecifier {
    case StorageClassSpecifier(value: AST.StorageClassSpecifier)
    case TypeSpecifier(value: AST.TypeSpecifier)
    case TypeQualifier(value: AST.TypeQualifier)
  }

  final case class TypeName(
      sqs: NonEmptyList[TypeSpecifierOrQualifier],
      abstractDeclarator: Option[AbstractDeclarator]
  ) {
    def specifiers: List[TypeSpecifier] = 
      sqs.collect {
        case TypeSpecifierOrQualifier.Specifier(ts) => ts
      }
    def qualifiers: List[TypeQualifier] = 
      sqs.collect {
        case TypeSpecifierOrQualifier.Qualifier(ts) => ts
      }
  }

  final case class AbstractDeclarator(pointer: Pointer)

  enum TypeSpecifierOrQualifier {
    case Specifier(s: TypeSpecifier)
    case Qualifier(q: TypeQualifier)
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
    case StructOrUnion(variant: AST.StructOrUnion, body: StructBody)
    case Union()
    case Enum()
    case TypedefName()
  }

  enum StructBody {
    case Incomplete(ident: Identifier)
    case Full(ident: Option[Identifier], decls: NonEmptyList[StructDeclaration])
  }

  enum StructOrUnion {
    case Struct
    case Union
  }

  final case class StructDeclaration(specifierQualifiers: NonEmptyList[TypeSpecifierOrQualifier], declarators: NonEmptyList[StructDeclarator])

  final case class StructDeclarator(declarator: Declarator)

  enum TypeQualifier {
    case Const
    case Volatile
  }

  // TODO: break apart Typable in our custom AST so the type is explicit
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
    case Declarator(specifiers: DeclarationSpecifiers, declarator: Option[AST.Declarator])
  }

  final case class Pointer(typeQualifiers: Option[NonEmptyList[TypeQualifier]], pointer: Option[Pointer])

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
              decl.get.identifier.get -> specs
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

  sealed trait Expression

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
    final case class Reference(expr: Expression) extends Expression
    final case class Dereference(op: Expression) extends Expression
    final case class Cast(typeName: TypeName, expr: Expression) extends Expression
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
