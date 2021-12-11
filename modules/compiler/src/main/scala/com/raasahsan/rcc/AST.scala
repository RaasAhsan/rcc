package com.raasahsan.rcc

object AST {
  // The unit of program text after preprocessing is called a translation unit,
  // which consists of a sequence of external declarations.
  final case class TranslationUnit(externalDeclarations: List[ExternalDeclaration])

  enum ExternalDeclaration {
    case FunctionDefinition()
    case Declaration()
  }

  final case class DeclarationList(declarations: List[Declaration])

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

  final case class DeclarationSpecifiers(specifiers: List[DeclarationSpecifier])

  enum DeclarationSpecifier {
    case StorageClass(value: StorageClassSpecifier)
    case Type(value: TypeSpecifier)
    case TypeQual(value: TypeQualifier)
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
