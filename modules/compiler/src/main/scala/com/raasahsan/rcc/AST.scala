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

  final case class StatementList(statements: List[Statement])

  enum Statement {
    case Labeled(value: LabeledStatement)
    case Compound(value: CompoundStatement)
    case Expression()
    case Selection()
    case Iteration()
    case Jump()
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

  enum TypeQualifier {
    case Const
    case Static
  }

  final case class InitDeclaratorList(declarators: List[InitDeclarator])

  final case class InitDeclarator(declarator: Declarator, initializer: Option[Initializer])

  final case class Declarator(pointer: Option[Pointer], directDeclarator: DirectDeclarator)

  final case class Pointer()

  final case class DirectDeclarator(identifier: Identifier)

  final case class Identifier(value: String)

  enum Initializer {
    case Assignment(expression: AssignmentExpression)
    case Initializers(initializers: InitializerList)
  }

  final case class InitializerList(initializers: List[Initializer])

  enum Expression {
    case Assignment(value: AssignmentExpression)
  }

  final case class AssignmentExpression()
}
