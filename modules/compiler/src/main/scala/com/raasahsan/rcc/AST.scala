package com.raasahsan.rcc

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
  )

  final case class DeclarationList(declarations: NonEmptyList[Declaration])

  final case class StatementList(statements: NonEmptyList[Statement])

  enum Statement {
    case Labeled(stmt: LabeledStatement)
    case Compound(stmt: CompoundStatement)
    case Expression()
    case Selection()
    case Iteration()
    case Jump(stmt: JumpStatement)
  }

  final case class LabeledStatement()

  final case class CompoundStatement(declarationList: Option[DeclarationList], statementList: Option[StatementList])

  // Unconditional jump to another location in the program
  enum JumpStatement {
    case Goto(id: Identifier)
    case Continue
    case Break
    case Return(expression: Option[Expression])
  }

  final case class Declaration(specifiers: DeclarationSpecifiers, initDeclaratorList: Option[InitDeclaratorList])

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

  final case class Declarator(pointer: Option[Pointer], directDeclarator: DirectDeclarator)

  final case class ParameterTypeList(parameterList: ParameterList, repeated: Boolean)

  final case class ParameterList(parameters: NonEmptyList[ParameterDeclaration])

  final case class IdentifierList(identifiers: NonEmptyList[Identifier])

  // TODO: abstract declarator
  enum ParameterDeclaration {
    case Declarator(specifiers: DeclarationSpecifiers, declarator: AST.Declarator)
  }

  final case class Pointer(typeQualifiers: Option[TypeQualifierList], pointer: Option[Pointer])

  enum DirectDeclarator {
    case Identifier(value: AST.Identifier)
    case Declarator(value: AST.Declarator)
    case FunctionDeclarator(decl: DirectDeclarator, parameterTypeList: ParameterTypeList)
    case Identifiers(decl: DirectDeclarator, identifiers: Option[IdentifierList])
  }

  final case class Identifier(value: String)

  enum Initializer {
    case Expression(expression: AST.Expression)
    case Initializers(initializers: InitializerList)
  }

  final case class InitializerList(initializers: NonEmptyList[Initializer])

  // enum Expression {
  //   case Assignment(value: AssignmentExpression)
  // }

  final case class AssignmentExpression(constant: Constant)

  enum Expression {
    case Constant(constant: AST.Constant)
    case Identifier(identifier: AST.Identifier)
    case Assignment(lhs: Expression, rhs: Expression)
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

}
