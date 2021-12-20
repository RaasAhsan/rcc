package com.raasahsan.rcc

import cats.syntax.all._
import cats.parse.{Parser => P, Parser0 => P0}

object Parser {

  import AST._

  def parse(input: String) =
    translationUnit.parseAll(input)

  def translationUnit: P[TranslationUnit] =
    maybeWhitespace.with1 *> externalDeclaration.rep
      .map(TranslationUnit(_))
      .withContext("translationUnit")

  def externalDeclaration: P[ExternalDeclaration] = (
    functionDefinition.map(ExternalDeclaration.FunctionDefinition(_)) |
      declaration.map(ExternalDeclaration.Declaration(_))
  ).withContext("externalDeclaration")

  // TODO: can't compose P.With1 with Parser1
  def functionDefinition: P[FunctionDefinition] =
    (declarationSpecifiers.?.with1 ~ declarator ~ declarationList.? ~ compoundStatement)
      .map { case (((specifiers, declarator), declarations), statements) =>
        FunctionDefinition(specifiers, declarator, declarations, statements)
      }
      .withContext("functionDefinition")

  def declarationList: P[DeclarationList] =
    declaration.rep.map(DeclarationList(_))

  def declaration: P[Declaration] =
    (declarationSpecifiers ~ initDeclaratorList.?).map { (specifiers, inits) =>
      Declaration(specifiers, inits)
    }

  def initDeclaratorList: P[InitDeclaratorList] =
    initDeclarator.rep.map(InitDeclaratorList(_))

  def initDeclarator: P[InitDeclarator] =
    (declarator ~ (assignOp *> initializer).?).map { case (declarator, init) =>
      InitDeclarator(declarator, init)
    }

  def initializer: P[Initializer] =
    assignmentExpression.map(Initializer.Expression(_))

  def declarationSpecifiers: P[DeclarationSpecifiers] =
    declarationSpecifier.rep.map(DeclarationSpecifiers(_))

  def declarationSpecifier: P[DeclarationSpecifier] =
    storageClassSpecifier.map(DeclarationSpecifier.StorageClassSpecifier(_)) |
      typeSpecifier.map(DeclarationSpecifier.TypeSpecifier(_)) |
      typeQualifier.map(DeclarationSpecifier.TypeQualifier(_))

  // TODO: whitespace?
  def storageClassSpecifier: P[StorageClassSpecifier] =
    keyword("typedef").as(StorageClassSpecifier.Typedef) |
      keyword("extern").as(StorageClassSpecifier.Extern) |
      keyword("static").as(StorageClassSpecifier.Static) |
      keyword("auto").as(StorageClassSpecifier.Auto) |
      keyword("register").as(StorageClassSpecifier.Register)

  // TODO: finish with composite types
  def typeSpecifier: P[TypeSpecifier] =
    keyword("void").as(TypeSpecifier.Void) |
      keyword("char").as(TypeSpecifier.Char) |
      keyword("short").as(TypeSpecifier.Short) |
      keyword("int").as(TypeSpecifier.Int) |
      keyword("long").as(TypeSpecifier.Long) |
      keyword("float").as(TypeSpecifier.Float) |
      keyword("double").as(TypeSpecifier.Double) |
      keyword("signed").as(TypeSpecifier.Signed) |
      keyword("unsigned").as(TypeSpecifier.Unsigned)

  def typeQualifierList: P[TypeQualifierList] =
    typeQualifier.rep.map(TypeQualifierList(_))

  def typeQualifier: P[TypeQualifier] =
    keyword("const").as(TypeQualifier.Const) |
      keyword("static").as(TypeQualifier.Static)

  def declarator: P[Declarator] =
    (pointer.?.with1 ~ directDeclarator)
      .map { case (pointer, decl) => Declarator(pointer, decl) }
      .withContext("declarator")

  // Definition of this nonterminal has some problems with recursive descent / LL parsers
  // TODO: we are special casing identifiers in arg/function declarators
  def directDeclarator: P[DirectDeclarator] = (
    withParentheses(P.defer(declarator)).map(DirectDeclarator.Declarator(_)) |
      (identifier.map(DirectDeclarator.Identifier(_)) ~ withParentheses(
        P.defer(parameterTypeList)
      )).backtrack.map((decl, params) => DirectDeclarator.FunctionDeclarator(decl, params)) |
      (identifier.map(DirectDeclarator.Identifier(_)) ~ withParentheses0(
        P.defer(identifierList).?
      )).backtrack.map((decl, ids) => DirectDeclarator.Identifiers(decl, ids)) |
      identifier.map(DirectDeclarator.Identifier(_))
  ).withContext("directDeclarator")

  def pointer: P[Pointer] =
    P.recursive { rec =>
      (asterisk *> typeQualifierList.?.with1 ~ rec).map { case (qualifiers, pointer) =>
        Pointer(qualifiers, Some(pointer))
      } |
        (asterisk *> typeQualifierList.?).map { qualifiers => Pointer(qualifiers, None) }
    }

  // TODO: fix
  def parameterTypeList: P[ParameterTypeList] =
    parameterList.map(ParameterTypeList(_, false))

  def parameterList: P[ParameterList] =
    parameterDeclaration.rep.map(ParameterList(_))

  def parameterDeclaration: P[ParameterDeclaration] =
    (declarationSpecifiers ~ declarator).map { (specifiers, declarator) =>
      ParameterDeclaration.Declarator(specifiers, declarator)
    }

  def identifierList: P[IdentifierList] =
    identifier.repSep(comma).map(IdentifierList(_))

  // Statements

  def statementList: P[StatementList] =
    statement.rep.map(StatementList(_))

  def statement: P[Statement] = (
    jumpStatement.map(Statement.Jump(_)) |
      compoundStatement.map(Statement.Compound(_)) |
      expressionStatement.map(Statement.Expression(_))
  ).withContext("statement")

  def expressionStatement: P[ExpressionStatement] = 
    (expression.?.with1 <* semicolon).map(ExpressionStatement(_))

  def jumpStatement: P[JumpStatement] =
    (returnKeyword *> expression.? <* semicolon).map(expr => JumpStatement.Return(expr))

  def compoundStatement: P[CompoundStatement] =
    (leftBrace *> P.defer0(declarationList.? ~ statementList.? <* rightBrace)).map {
      (declarations, statements) =>
        CompoundStatement(declarations, statements)
    }

  def withParentheses[A](p: P[A]): P[A] =
    leftParentheses *> p <* rightParentheses

  def withParentheses0[A](p: P0[A]): P[A] =
    leftParentheses *> p <* rightParentheses

  /////////////////
  // Expressions //
  /////////////////

  def expression: P[Expression] =
    assignmentExpression

  def assignmentExpression: P[Expression] =
    P.recursive { rec =>
      additiveExpression |
        (primaryExpression ~ (assignOp *> rec)).map((l, r) => Expression.Assignment(l, r))
    }

  def additiveExpression: P[Expression] = {
    enum Op {
      case Plus
      case Minus
    }

    import Op._

    (multiplicativeExpression ~ ((plus.as(Plus) | minus.as(Minus)) ~ multiplicativeExpression).rep0).map { (h, t) =>
      t.foldLeft(h) { case (acc, (op, expr)) =>
        op match {
          case Plus => Expression.Plus(acc, expr)
          case Minus => Expression.Minus(acc, expr)
        }
      }
    }
  }

  def multiplicativeExpression: P[Expression] = {
    enum Op {
      case Star
      case Divide
      case Modulo
    }

    import Op._

    (primaryExpression ~ ((star.as(Star) | divide.as(Divide) | modulo.as(Modulo)) ~ primaryExpression).rep0).map { (h, t) =>
      t.foldLeft(h) { case (acc, (op, expr)) =>
        op match {
          case Star => Expression.Times(acc, expr)
          case Divide => Expression.Divide(acc, expr)
          case Modulo => Expression.Modulo(acc, expr)
        }
      }
    }
  }

  def primaryExpression: P[Expression] =
    constant.map(Expression.Constant(_)) |
      identifier.map(Expression.Identifier(_)) |
      withParentheses(P.defer(expression))

  def assignmentOperator: P[AssignmentOperator] =
    operator("=").as(AssignmentOperator.Assign) |
      operator("*=").as(AssignmentOperator.StarAssign) |
      operator("/=").as(AssignmentOperator.DivAssign) |
      operator("%=").as(AssignmentOperator.ModAssign) |
      operator("+=").as(AssignmentOperator.StarAssign) |
      operator("-=").as(AssignmentOperator.MinusAssign) |
      operator("<<=").as(AssignmentOperator.ShlAssign) |
      operator(">>=").as(AssignmentOperator.ShrAssign) |
      operator("&=").as(AssignmentOperator.AndAssign) |
      operator("^=").as(AssignmentOperator.XorAssign) |
      operator("|=").as(AssignmentOperator.OrAssign)

  // Lexical elements
  // all whitespace handling is performed at terminals
  // higher-level parsers must reference these

  def constant: P[Constant] =
    integerConstant.map(Constant.IntegerConstant(_)) <* maybeWhitespace

  // TODO: refine with other types of constants
  def integerConstant: P[Int] =
    decimalConstant

  def decimalConstant: P[Int] =
    (nonzeroDigit ~ digit.rep0).map { (h, t) =>
      (h :: t).mkString.toInt
    }

  def keyword(t: String): P[Unit] =
    P.string(t) <* maybeWhitespace

  def operator(t: String): P[Unit] =
    P.string(t) <* maybeWhitespace

  def identifier: P[Identifier] =
    (nondigit ~ (digit | nondigit).rep0).map { case (c, cs) =>
      Identifier((c :: cs).mkString)
    } <* maybeWhitespace

  def digit: P[Char] = P.charIn(('0' to '9').toList)

  def nonzeroDigit: P[Char] = P.charIn(('1' to '9').toList)

  def nondigit: P[Char] = P.charIn('_' :: ('a' to 'z').toList ::: ('A' to 'Z').toList)

  def leftParentheses: P[Unit] = operator("(")

  def rightParentheses: P[Unit] = operator(")")

  def leftBrace: P[Unit] = operator("{")

  def rightBrace: P[Unit] = operator("}")

  def asterisk: P[Unit] =
    operator("*")

  def whitespace: P[Unit] =
    P.charsWhile(_.isWhitespace).void

  def maybeWhitespace: P0[Unit] =
    whitespace.?.void

  def assignOp: P[Unit] = operator("=")
  def semicolon: P[Unit] = operator(";")
  def comma: P[Unit] = operator(",")
  def plus: P[Unit] = operator("+")
  def minus: P[Unit] = operator("-")
  def star: P[Unit] = operator("*")
  def divide: P[Unit] = operator("/")
  def modulo: P[Unit] = operator("%")

  def returnKeyword: P[Unit] =
    keyword("return")

}
