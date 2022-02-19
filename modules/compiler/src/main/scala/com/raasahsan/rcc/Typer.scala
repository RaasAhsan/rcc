package com.raasahsan.rcc

import cats.syntax.all._

// TODO: only typing semantics or other semantics here? like unique declarations
object Typer {

  import AST._

  type TypingContext = Map[Identifier, Type]

  def typeCheck(unit: TranslationUnit): Either[String, TypingContext] =
    unit.externalDeclarations.toList
      .foldLeftM(Map[Identifier, Type]()) { case (ctx, ed) =>
        ed match {
          case ExternalDeclaration.FunctionDefinition(fd) =>
            typeCheckFunctionDefinition(fd, ctx)
          case ExternalDeclaration.Declaration(decl) =>
            typeCheckDeclaration(decl, ctx)
        }
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
    } yield {
      val tpe = Type.Function(argTpes.map(_._2), returnTpe)
      fd.tpe = Some(tpe)
      ctx + (ident -> tpe)
    }

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

          val nextTpe = init.declarator.directDeclarator match {
            case DirectDeclarator.Identifier(ident) =>
              init.initializer match {
                case Some(Initializer.Expression(expr)) =>
                  typeCheckExpression(expr, ctx).flatMap { itpe =>
                    if (itpe == tpe) Right(ident -> tpe)
                    else Left("Initializer types did not match")
                  }
                case _ => Right(ident -> tpe)
              }
            case DirectDeclarator.FunctionDeclarator(decl, params) =>
              for {
                ident <- decl.identifier.fold(Left("ident not found"))(Right(_))
                paramTypes <- params.parameterList.parameters.toList
                  .map { case ParameterDeclaration.Declarator(specs, decl) =>
                    specs -> decl.get.pointer
                  }
                  .traverse { case (specifiers, pointer) =>
                    specifiersToType(specifiers)
                      .fold(Left("no argument type found"))(Right(_))
                      .map(tpe => pointer.map(_ => Type.Pointer(tpe)).getOrElse(tpe))
                  }
              } yield ident -> Type.Function(paramTypes, tpe)
            case _ => Left("unknown declaration")
          }

          init.tpe = nextTpe.toOption.map(_._2)

          nextTpe.map { mapping =>
            ctx + mapping
          }
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
      case Expression.Minus(l, r) =>
        for {
          lt <- typeCheckExpression(l, ctx)
          rt <- typeCheckExpression(r, ctx)
          // TODO: + is valid for all arithmetic types
          _ <-
            if (lt == rt && rt == Type.Int) Right(())
            else Left("Expected int on both sides of plus")
        } yield lt
      case Expression.Times(l, r) =>
        for {
          lt <- typeCheckExpression(l, ctx)
          rt <- typeCheckExpression(r, ctx)
          // TODO: + is valid for all arithmetic types
          _ <-
            if (lt == rt && rt == Type.Int) Right(())
            else Left("Expected int on both sides of plus")
        } yield lt
      case Expression.Divide(l, r) =>
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
      case Expression.FunctionCall(func, args) =>
        for {
          tpe <- typeCheckExpression(func, ctx)
          fty <- tpe match {
            case Type.Function(pts, rt) => Right(pts -> rt)
            case _                      => Left("function type expected")
          }
          paramTypes <- args
            .map(_.args.toList)
            .getOrElse(Nil)
            .traverse(expr => typeCheckExpression(expr, ctx))
          _ <-
            if (paramTypes == fty._1) Right(())
            else Left("function type and arguments do not match")
        } yield fty._2
      case Expression.Reference(expr) =>
        for {
          tpe <- typeCheckExpression(expr, ctx)
          _ <-
            if (isLvalue(expr)) Right(())
            else Left("modifiable lvalue expected for unary referencing")
        } yield Type.Pointer(tpe)
      case Expression.Dereference(expr) =>
        for {
          tpe <- typeCheckExpression(expr, ctx)
          _ <-
            if (isLvalue(expr)) Right(())
            else Left("modifiable lvalue expected for unary dereferencing")
          utpe <- tpe match {
            case Type.Pointer(u) => Right(u)
            case _               => Left("pointer type expected")
          }
        } yield utpe
      case Expression.Cast(typeName, expr) =>
        for {
          tpe <- typeCheckExpression(expr, ctx)
          castTpe <- typeNameToType(typeName).fold(Left("invalid type"))(Right(_))
          _ <- compatibleCast(tpe, castTpe)
        } yield castTpe
      case x => Left(s"invalid expression $x")
    }

    expr.tpe = tpe.toOption

    tpe
  }

  def compatibleCast(source: Type, target: Type): Either[String, Unit] =
    (target, source) match {
      case (Type.Pointer(_), Type.Int) => Right(())
      case _ if source == target       => Right(())
      case _                           => Left("invalid cast")
    }

  def isLvalue(expr: Expression): Boolean =
    expr match {
      case Expression.Identifier(_) => true
      case _                        => false
    }

  val specifierMapping: Map[Set[TypeSpecifier], Type] = Map(
    Set(TypeSpecifier.Int) -> Type.Int,
    Set(TypeSpecifier.Char) -> Type.Char,
    Set(TypeSpecifier.Unsigned, TypeSpecifier.Int) -> Type.UnsignedInt
  )

  def typePointer(tpe: Type, pointer: Option[Pointer]): Type =
    pointer.fold(tpe)(_ => Type.Pointer(tpe))

  def specifiersToType(specifiers: DeclarationSpecifiers): Option[Type] = {
    val key = specifiers.specifiers.toList.collect { case DeclarationSpecifier.TypeSpecifier(ts) =>
      ts
    }.toSet
    specifierMapping.get(key)
  }

  def typeNameToType(typeName: TypeName): Option[Type] = {
    val key = typeName.specifierQualifiers.toList.collect {
      case TypeSpecifierOrQualifier.Specifier(s) => s
    }.toSet
    specifierMapping.get(key).map(b => typeName.abstractDeclarator.fold(b)(_ => Type.Pointer(b)))
  }

}
