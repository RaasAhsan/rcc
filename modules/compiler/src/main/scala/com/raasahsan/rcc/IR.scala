package com.raasahsan.rcc

import cats.syntax.all._
import cats.data.NonEmptyList

object IR {
  // The unit of program text after preprocessing is called a translation unit,
  // which consists of a sequence of external declarations.
  final case class Module(moduleDeclarations: List[ModuleDeclaration])

  enum ModuleDeclaration {
    case FunctionDefinition(value: IR.FunctionDefinition)
    case FunctionDeclaration(value: IR.FunctionDeclaration)
    case Declaration(value: IR.Declaration)
  }

  final case class FunctionDeclaration()

  final case class FunctionDefinition(
      // At most one storage class specifier can be provided in a declaration/function definition
      storageClass: Option[StorageClassSpecifier],
      name: Identifier,
      returnTpe: Type, // TODO: QualifiedType?
      parameters: Option[List[FunctionParameter]],
      block: Block
  ) extends Typable

  final case class FunctionParameter(tpe: Type, name: Identifier)

  final case class Block(declarations: List[Declaration], statements: List[Statement])

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
      declarations: List[Declaration],
      statements: List[Statement]
  )

  // Unconditional jump to another location in the program
  enum JumpStatement {
    case Goto(id: Identifier)
    case Continue
    case Break
    case Return(expression: Option[Expression])
  }

  final case class Declaration(
      storageClass: Option[StorageClassSpecifier],
      qualifiers: List[TypeQualifier],
      name: Identifier,
      tpe: Type,
      initializer: Option[Initializer]
  )

  final case class DeclarationSpecifiers(specifiers: NonEmptyList[DeclarationSpecifier])

  enum DeclarationSpecifier {
    case StorageClassSpecifier(value: IR.StorageClassSpecifier)
    case TypeSpecifier(value: IR.TypeSpecifier)
    case TypeQualifier(value: IR.TypeQualifier)
  }

  final case class TypeName(
      specifierQualifiers: NonEmptyList[TypeSpecifierOrQualifier],
      abstractDeclarator: Option[AbstractDeclarator]
  )

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
    case StructOrUnion()
    case Enum()
    case TypedefName()
  }

  enum TypeQualifier {
    case Const
    case Volatile
  }

  final case class Declarator(pointer: Option[Pointer], directDeclarator: DirectDeclarator)

  final case class ParameterTypeList(parameterList: ParameterList, repeated: Boolean)

  final case class ParameterList(parameters: NonEmptyList[ParameterDeclaration])

  final case class IdentifierList(identifiers: NonEmptyList[Identifier])

  // TODO: abstract declarator
  enum ParameterDeclaration {
    case Declarator(specifiers: DeclarationSpecifiers, declarator: IR.Declarator)
  }

  final case class Pointer(typeQualifiers: List[TypeQualifier], pointer: Option[Pointer])

  enum DirectDeclarator {
    case Identifier(value: IR.Identifier)
    case Declarator(value: IR.Declarator)
    case FunctionDeclarator(decl: DirectDeclarator, parameterTypeList: ParameterTypeList)
    case Identifiers(decl: DirectDeclarator, identifiers: Option[IdentifierList])
  }

  final case class Identifier(value: String)

  enum Initializer {
    case Expression(expression: IR.Expression)
    case Initializers(initializers: InitializerList)
  }

  final case class InitializerList(initializers: NonEmptyList[Initializer])

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

  // This Expression AST doesn't reflect the formal grammar of C,
  // however we are flattening it here for simplicity
  sealed trait Expression extends Typable

  object Expression {
    final case class Constant(constant: IR.Constant) extends Expression
    final case class Identifier(identifier: IR.Identifier) extends Expression
    final case class StringLiteral(literal: String) extends Expression
    final case class Assignment(lhs: Expression, rhs: Expression) extends Expression
    final case class Plus(lhs: Expression, rhs: Expression) extends Expression
    final case class Minus(lhs: Expression, rhs: Expression) extends Expression
    final case class Times(lhs: Expression, rhs: Expression) extends Expression
    final case class Divide(lhs: Expression, rhs: Expression) extends Expression
    final case class Modulo(lhs: Expression, rhs: Expression) extends Expression
    final case class FunctionCall(lhs: Expression, args: List[Expression]) extends Expression
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

}
