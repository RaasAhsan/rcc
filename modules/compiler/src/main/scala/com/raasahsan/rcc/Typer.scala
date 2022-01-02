package com.raasahsan.rcc

import cats.syntax.all._

// TODO: only typing semantics or other semantics here? like unique declarations
object Typer {

  import AST._

  type TypingContext = Map[Identifier, Type]

  def typeCheck(unit: TranslationUnit): Either[String, TypingContext] =
    unit.externalDeclarations.toList
      .collect { case ExternalDeclaration.FunctionDefinition(fd) =>
        fd
      }
      .foldLeftM(Map[Identifier, Type]()) { case (ctx, fd) =>
        typeCheckFunctionDefinition(fd, ctx)
      }

  def typeCheckFunctionDefinition(
      fd: FunctionDefinition,
      ctx: Map[Identifier, Type]
  ): Either[String, TypingContext] =
    for {
      baseReturnTpe <- fd.specifiers.flatMap(specifiersToType).fold(Left("no type found"))(Right(_))
      returnTpe = typePointer(baseReturnTpe, fd.declarator.pointer)
      // TODO: improve this
      ident <- fd.functionName.fold(Left("no function name"))(Right(_))
      params <- fd.functionParameters.fold(Left("no function parameters"))(Right(_))
      argTpes <- params.traverse { case (ident, specifiers) =>
        specifiersToType(specifiers)
          .map(tpe => ident -> tpe)
          .fold(Left("no argument type found"))(Right(_))
      }
      innerCtx1 = ctx ++ argTpes.toMap
      innerCtx2 <- fd.declarationList.fold(Right(innerCtx1))(decls =>
        typeCheckDeclarations(decls, innerCtx1)
      )
      _ <- typeCheckStatement(Statement.Compound(fd.statements), innerCtx2)
    } yield ctx + (ident -> Type.Function(argTpes.map(_._2), returnTpe))

  def typeCheckDeclarations(
      decls: DeclarationList,
      ctx: Map[Identifier, Type]
  ): Either[String, TypingContext] =
    decls.declarations.toList.foldLeftM(ctx) { (acc, decl) =>
      typeCheckDeclaration(decl, acc)
    }

  // TODO: Option is just nice for composability
  def typeCheckStatements(
      stmts: StatementList,
      ctx: Map[Identifier, Type]
  ): Either[String, TypingContext] =
    stmts.statements.toList.foldLeftM(ctx) { (acc, stmt) =>
      typeCheckStatement(stmt, acc)
    }

  val specifierMapping: Map[Set[TypeSpecifier], Type] = Map(
    Set(TypeSpecifier.Int) -> Type.Int,
    Set(TypeSpecifier.Char) -> Type.Char
  )

  def specifiersToType(specifiers: DeclarationSpecifiers): Option[Type] = {
    val key = specifiers.specifiers.toList.collect { case DeclarationSpecifier.TypeSpecifier(ts) =>
      ts
    }.toSet
    specifierMapping.get(key)
  }

  def typePointer(tpe: Type, pointer: Option[Pointer]): Type =
    pointer.fold(tpe)(_ => Type.Pointer(tpe))

  // TODO: this only works for block-level declarations, not functions?
  def typeCheckDeclaration(decl: Declaration, ctx: TypingContext): Either[String, TypingContext] =
    for {
      baseTpe <- specifiersToType(decl.specifiers).fold(Left("no type found via specifiers"))(
        Right(_)
      )
      nextCtx <- decl.initDeclaratorList.map(_.declarators.toList).getOrElse(Nil).foldLeftM(ctx) {
        (acc, init) =>
          val tpe = init.declarator.pointer.map(_ => Type.Pointer(baseTpe)).getOrElse(baseTpe)
          // TODO: a declaration declares an identifier at most ONCE
          for {
            ident <- init.declarator.identifier.fold(Left("no identifier found in declarator"))(Right(_))
            // TODO: some combinator that only validates when a value is present
            _ <- init.initializer match {
              case Some(Initializer.Expression(expr)) => 
                typeCheckExpression(expr, ctx).flatMap { itpe =>
                  if (itpe == tpe) Right(()) else Left("Initializer types did not match")
                }
              case _ => Right(())
            }
          } yield acc + (ident -> tpe)
      }
    } yield nextCtx

  def typeCheckStatement(stmt: Statement, ctx: TypingContext): Either[String, TypingContext] =
    stmt match {
      case Statement.Compound(compound) =>
        for {
          ctx1 <- compound.declarationList.fold(Right(ctx)) { decls =>
            typeCheckDeclarations(decls, ctx)
          }
          _ <- compound.statementList.fold(Right(ctx1)) { statements =>
            typeCheckStatements(statements, ctx1)
          }
        } yield ctx
      case Statement.Expression(expr) =>
        expr.expr match {
          case Some(e) => typeCheckExpression(e, ctx).as(ctx)
          case None    => Right(ctx)
        }
      case Statement.Selection(select) =>
        select match {
          case SelectionStatement.If(cond, con, alt) =>
            for {
              ct <- typeCheckExpression(cond, ctx)
              _ <- if (ct == Type.Int) Right(()) else Left("if type boolean expected")
              _ <- typeCheckStatement(con, ctx)
              _ <- alt match {
                case Some(stmt) => typeCheckStatement(stmt, ctx)
                case None       => Right(())
              }
            } yield ctx
        }
      case Statement.Jump(jump) =>
        jump match {
          case JumpStatement.Return(ret) =>
            // TODO: match return type of function with type here
            val res = ret match {
              case Some(expr) => typeCheckExpression(expr, ctx)
              case None       => Right(Type.Void)
            }
            res.as(ctx)
          case _ => ???
        }
      case _ => ???
    }

  // Typing expressions should never yield a new context
  def typeCheckExpression(expr: Expression, ctx: TypingContext): Either[String, Type] = {
    val tpe = expr match {
      case Expression.Constant(const) =>
        const match {
          case Constant.IntegerConstant(_) => Right(Type.Int)
        }
      case Expression.StringLiteral(_) => Right(Type.Pointer(Type.Char))
      case Expression.Plus(l, r) =>
        for {
          lt <- typeCheckExpression(l, ctx)
          rt <- typeCheckExpression(r, ctx)
          // TODO: + is valid for all arithmetic types
          _ <-
            if (lt == rt && rt == Type.Int) Right(())
            else Left("Expected int on both sides of plus")
        } yield lt
      case Expression.Identifier(ident) =>
        ctx.get(ident).fold(Left(s"identifier $ident not found"))(Right(_))
      case Expression.Assignment(l, r) =>
        for {
          lt <- typeCheckExpression(l, ctx)
          rt <- typeCheckExpression(r, ctx)
          _ <- if (lt == rt) Right(()) else Left("both sides of expression must type same")
        } yield lt
      case _ => Left("invalid expression")
    }

    expr.tpe = tpe.toOption

    tpe
  }

}
