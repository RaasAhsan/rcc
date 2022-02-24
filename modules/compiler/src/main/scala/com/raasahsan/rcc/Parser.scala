package com.raasahsan.rcc

import cats.data.NonEmptyList
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
    functionDefinition.map(ExternalDeclaration.FunctionDefinition(_)).backtrack |
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
    (declarationSpecifiers ~ initDeclaratorList.? <* semicolon).map { (specifiers, inits) =>
      Declaration(specifiers, inits)
    }

  def initDeclaratorList: P[NonEmptyList[InitDeclarator]] =
    initDeclarator.repSep(comma)

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

  def typeName: P[TypeName] =
    (typeSpecifierOrQualifiers ~ abstractDeclarator.?).map { (ts, ad) =>
      TypeName(ts, ad)
    }

  def abstractDeclarator: P[AbstractDeclarator] =
    pointer.map(AbstractDeclarator(_))

  def typeSpecifierOrQualifiers: P[TypeSpecifierOrQualifiers] =
    typeSpecifierOrQualifier.rep.map(TypeSpecifierOrQualifiers(_))

  def typeSpecifierOrQualifier: P[TypeSpecifierOrQualifier] =
    typeSpecifier.map(TypeSpecifierOrQualifier.Specifier(_)) |
      typeQualifier.map(TypeSpecifierOrQualifier.Qualifier(_))

  // TODO: we need to place some constraints on what is allowed here
  // struct-or-union cannot be mixed with int for example, which is causing some problems
  def typeSpecifier: P[TypeSpecifier] =
    keyword("void").as(TypeSpecifier.Void) |
      keyword("char").as(TypeSpecifier.Char) |
      keyword("short").as(TypeSpecifier.Short) |
      keyword("int").as(TypeSpecifier.Int) |
      keyword("long").as(TypeSpecifier.Long) |
      keyword("float").as(TypeSpecifier.Float) |
      keyword("double").as(TypeSpecifier.Double) |
      keyword("signed").as(TypeSpecifier.Signed) |
      keyword("unsigned").as(TypeSpecifier.Unsigned) |
      structOrUnionSpecifier

  def structOrUnionSpecifier: P[TypeSpecifier] =
    (structOrUnion ~ identifier.? ~ P.defer(leftBrace *> structDeclarationList <* rightBrace))
      .map { case ((su, ident), decls) =>
        TypeSpecifier.StructOrUnion(su, StructBody.Full(ident, decls))
      } |
      (structOrUnion ~ identifier).map { case (su, ident) =>
        TypeSpecifier.StructOrUnion(su, StructBody.Incomplete(ident))
      }

  def structOrUnion: P[StructOrUnion] =
    keyword("struct").as(StructOrUnion.Struct) |
      keyword("union").as(StructOrUnion.Union)

  def structDeclarationList: P[NonEmptyList[StructDeclaration]] =
    structDeclaration.rep

  def structDeclaration: P[StructDeclaration] =
    (typeSpecifierOrQualifiers ~ structDeclaratorList <* semicolon).map { case (sqs, declarators) =>
      StructDeclaration(sqs, declarators)
    }

  def structDeclaratorList: P[NonEmptyList[StructDeclarator]] =
    structDeclarator.repSep(comma)

  def structDeclarator: P[StructDeclarator] =
    declarator.map(StructDeclarator(_))

  def typeQualifierList: P[NonEmptyList[TypeQualifier]] =
    typeQualifier.rep

  def typeQualifier: P[TypeQualifier] =
    keyword("const").as(TypeQualifier.Const) |
      keyword("volatile").as(TypeQualifier.Volatile)

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
    P.recursive { recurse =>
      (asterisk *> (typeQualifierList.? ~ recurse.?)).map { case (qualifiers, pointer) =>
        Pointer(qualifiers, pointer)
      }
    }

  // TODO: fix
  def parameterTypeList: P[ParameterTypeList] =
    parameterList.map(ParameterTypeList(_, false))

  def parameterList: P[ParameterList] =
    parameterDeclaration.repSep(comma).map(ParameterList(_))

  // TODO: abstract declarator is necessary here
  def parameterDeclaration: P[ParameterDeclaration] =
    (declarationSpecifiers ~ declarator.?).map { (specifiers, declarator) =>
      ParameterDeclaration.Declarator(specifiers, declarator)
    }

  def identifierList: P[IdentifierList] =
    identifier.repSep(comma).map(IdentifierList(_))

  ////////////////
  // Statements //
  ////////////////

  def statementList: P[StatementList] =
    statement.rep.map(StatementList(_))

  def statement: P[Statement] = (
    jumpStatement.map(Statement.Jump(_)).backtrack |
      expressionStatement.map(Statement.Expression(_)).backtrack |
      compoundStatement.map(Statement.Compound(_)).backtrack |
      selectionStatement.map(Statement.Selection(_))
  ).withContext("statement")

  def expressionStatement: P[ExpressionStatement] =
    (expression.?.with1 <* semicolon).map(ExpressionStatement(_)).withContext("expressionStatement")

  def jumpStatement: P[JumpStatement] =
    (returnKeyword *> expression.? <* semicolon).map(expr => JumpStatement.Return(expr))

  def compoundStatement: P[CompoundStatement] =
    (leftBrace *> P.defer0(declarationList.? ~ statementList.? <* rightBrace))
      .map { (declarations, statements) =>
        CompoundStatement(declarations, statements)
      }
      .withContext("compoundStatement")

  def selectionStatement: P[SelectionStatement] =
    (ifKeyword *> (leftParentheses *> (expression <* rightParentheses)) ~ P.defer(
      statement
    ) ~ (elseKeyword *> P.defer(statement)).?).map { case ((condition, consequent), alternative) =>
      SelectionStatement.If(condition, consequent, alternative)
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
      .withContext("expression")

  def assignmentExpression: P[Expression] =
    P.recursive[Expression] { rec =>
      additiveExpression |
        (primaryExpression ~ (assignOp *> rec)).map((l, r) => Expression.Assignment(l, r))
    }.withContext("assignmentExpression")

  def additiveExpression: P[Expression] = {
    enum Op {
      case Plus
      case Minus
    }

    import Op._

    (multiplicativeExpression ~ ((plus.as(Plus) | minus.as(Minus)) ~ multiplicativeExpression).rep0)
      .map { (h, t) =>
        t.foldLeft(h) { case (acc, (op, expr)) =>
          op match {
            case Plus  => Expression.Plus(acc, expr)
            case Minus => Expression.Minus(acc, expr)
          }
        }
      }
      .withContext("additiveExpression")
  }

  def multiplicativeExpression: P[Expression] = {
    enum Op {
      case Star
      case Divide
      case Modulo
    }

    import Op._

    def op: P[Op] =
      star.as(Star) | divide.as(Divide) | modulo.as(Modulo)

    (castExpression ~ (op ~ castExpression).rep0).map { (h, t) =>
      t.foldLeft(h) { case (acc, (op, expr)) =>
        op match {
          case Star   => Expression.Times(acc, expr)
          case Divide => Expression.Divide(acc, expr)
          case Modulo => Expression.Modulo(acc, expr)
        }
      }
    }
  }

  def castExpression: P[Expression] =
    (withParentheses(typeName) ~ P.defer(castExpression)).map { (tn, expr) =>
      Expression.Cast(tn, expr)
    }.backtrack | unaryExpression

  def unaryExpression: P[Expression] = {
    enum Op {
      case Reference
      case Dereference
    }

    import Op._

    def op: P[Op] =
      star.as(Dereference) |
        ampersand.as(Reference)

    // TODO: different uses of cast and unary expression must be allowed here
    postfixExpression.backtrack | (op ~ P.defer(castExpression)).map { (op, expr) =>
      op match {
        case Reference   => Expression.Reference(expr)
        case Dereference => Expression.Dereference(expr)
      }
    }
  }

  def postfixExpression: P[Expression] = {
    enum Op {
      case FunctionCall(args: Option[ArgumentExpressionList])
      case ArrayGet(index: Expression)
    }

    import Op._

    def op: P[Op] =
      (leftBracket *> P.defer(expression) <* rightBracket).map(ArrayGet(_)) |
        (leftParentheses *> P.defer0(argumentExpressionList.?) <* rightParentheses)
          .map(FunctionCall(_))

    (primaryExpression ~ op.rep0).map { (h, t) =>
      t.foldLeft(h) { case (acc, op) =>
        op match {
          case FunctionCall(args) => Expression.FunctionCall(acc, args)
          case ArrayGet(index)    => Expression.ArrayGet(acc, index)
        }
      }
    }
  }

  def primaryExpression: P[Expression] =
    constant.map(Expression.Constant(_)) |
      identifier.map(Expression.Identifier(_)) |
      stringLiteral.map(Expression.StringLiteral(_)) |
      withParentheses(P.defer(expression))

  def argumentExpressionList: P[ArgumentExpressionList] =
    assignmentExpression.repSep(comma).map(ArgumentExpressionList(_))

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

  // String literals
  def stringLiteral: P[StringLiteral] =
    (stringLiteralInit *> sCharSequence.? <* stringLiteralTerminator).map(cs =>
      StringLiteral(cs.getOrElse(""))
    )

  def sCharSequence: P[String] =
    (sChar ~ sChar.rep0).map((h, t) => (h :: t).mkString)

  // TODO: charset divergences between java and C?
  def sChar: P[Char] =
    P.anyChar.filter(c => c != '"' && c != '\n' && c != '\\').backtrack

  // TODO: implement character sequences
  def escapeSequence: P[Char] = P.fail

  // Lexical elements
  // all whitespace handling is performed at terminals
  // higher-level parsers must reference these

  def constant: P[Constant] =
    integerConstant.map(Constant.IntegerConstant(_)) <* maybeWhitespace

  def integerConstant: P[Int] =
    decimalConstant.backtrack |
      hexadecimalConstant.backtrack | // parse hex first since there is a common prefix
      octalConstant

  def decimalConstant: P[Int] =
    (nonzeroDigit ~ digit.rep0).map { (h, t) =>
      (h :: t).mkString.toInt
    }

  def octalConstant: P[Int] =
    zero.as(0)

  def hexadecimalConstant: P[Int] =
    ((P.string("0x") | P.string("0X")) *> hexadecimalDigit.rep).map { digits =>
      Integer.parseInt(digits.toList.mkString, 16)
    }

  def keyword(t: String): P[Unit] =
    P.string(t) <* maybeWhitespace

  def operator(t: String): P[Unit] =
    P.string(t) <* maybeWhitespace

  val keywords = List("int", "return")

  def identifier: P[Identifier] =
    (nondigit ~ (digit | nondigit).rep0 <* maybeWhitespace)
      .map((c, cs) => (c :: cs).mkString)
      // .filterNot(keywords.contains(_))
      .map(Identifier(_))

  def digit: P[Char] = P.charIn(('0' to '9').toList)
  def hexadecimalDigit: P[Char] =
    P.charIn(('0' to '9').toList ++ ('A' to 'F').toList ++ ('a' to 'f').toList)
  def nonzeroDigit: P[Char] = P.charIn(('1' to '9').toList)
  def zero: P[Unit] = P.char('0')
  def nondigit: P[Char] = P.charIn('_' :: ('a' to 'z').toList ::: ('A' to 'Z').toList)

  def leftParentheses: P[Unit] = operator("(")
  def rightParentheses: P[Unit] = operator(")")
  def leftBrace: P[Unit] = operator("{")
  def rightBrace: P[Unit] = operator("}")
  def leftBracket: P[Unit] = operator("[")
  def rightBracket: P[Unit] = operator("]")

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
  def ampersand: P[Unit] = operator("&")

  def returnKeyword: P[Unit] = keyword("return")
  def ifKeyword: P[Unit] = keyword("if")
  def elseKeyword: P[Unit] = keyword("else")

  def stringLiteralInit: P[Unit] = P.string("\"").void
  def stringLiteralTerminator: P[Unit] = operator("\"").void

}
