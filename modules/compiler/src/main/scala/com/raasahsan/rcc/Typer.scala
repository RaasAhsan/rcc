package com.raasahsan.rcc

import cats.syntax.all._

// TODO: only typing semantics or other semantics here? like unique declarations
object Typer {

  import IR._

  final case class TypingContext(
      names: Map[Identifier, Type],
      userDefinedTypes: Map[Identifier, UserDefinedType]
  ) {
    def addName(ident: Identifier, tpe: Type): TypingContext =
      copy(names = names + (ident -> tpe))
    def getName(ident: Identifier): Option[Type] = names.get(ident)
    def addUserDefinedType(ident: Identifier, tpe: UserDefinedType): TypingContext =
      copy(userDefinedTypes = userDefinedTypes + (ident -> tpe))
    def getUserDefinedType(ident: Identifier): Option[UserDefinedType] = userDefinedTypes.get(ident)

    def validType(tpe: Type): Either[String, Unit] =
      tpe match {
        case Type.UserDefined(ident) =>
          if (userDefinedTypes.contains(ident)) Right(()) else Left(s"$ident type is undefined")
        case _ => Right(())
      }
  }

  object TypingContext {
    val Init = TypingContext(Map(), Map())
  }

  enum UserDefinedType {
    case Struct(decl: IR.StructDeclaration)
  }

  def typeCheck(m: Module): Either[String, TypingContext] =
    for {
      ctx1 <- m.structDeclarations.toList
        .foldLeftM(TypingContext.Init) { case (ctx, sd) =>
          typeCheckStructDeclaration(sd, ctx)
        }
      ctx2 <- m.moduleDeclarations.toList
        .foldLeftM(ctx1) { case (ctx, ed) =>
          ed match {
            case ModuleDeclaration.FunctionDefinition(fd) =>
              typeCheckFunctionDefinition(fd, ctx).map(tpe => ctx.addName(fd.name, tpe))
            case ModuleDeclaration.Declaration(decl) =>
              typeCheckDeclaration(decl, ctx).map(tpe => ctx.addName(decl.name, tpe))
            case x => throw new RuntimeException(s"not implemented for $x")
          }
        }
    } yield ctx2

  def typeCheckStructDeclaration(
      struct: StructDeclaration,
      ctx: TypingContext
  ): Either[String, TypingContext] =
    struct.fields
      .foldLeftM(Set[Identifier]()) { case (acc, fieldDecl) =>
        for {
          _ <-
            if (acc.contains(fieldDecl.ident)) Left(s"${fieldDecl.ident} already declared")
            else Right(())
          _ <- ctx.validType(fieldDecl.tpe)
        } yield acc + fieldDecl.ident
      }
      .map(_ => ctx.addUserDefinedType(struct.ident, UserDefinedType.Struct(struct)))

  def typeCheckFunctionDefinition(
      fd: FunctionDefinition,
      ctx: TypingContext
  ): Either[String, Type] =
    for {
      _ <- ctx.validType(fd.returnTpe)
      _ <- fd.parameters.getOrElse(Nil).traverse(p => ctx.validType(p.tpe))
      params <- fd.parameters.fold(Left("no function parameters"))(Right(_))
      paramTpes = params.map { p => p.name -> p.tpe }
      _ <- typeCheckBlock(
        fd.block,
        paramTpes.foldLeft(ctx) { case (acc, (name, tpe)) => acc.addName(name, tpe) }
      )
    } yield {
      val tpe = Type.Function(paramTpes.map(_._2), fd.returnTpe)
      fd.tpe = Some(tpe)
      tpe
    }

  def typeCheckBlock(block: Block, ctx: TypingContext): Either[String, Unit] =
    for {
      ctx2 <- block.declarations.foldLeftM(ctx) { (ctx, decl) =>
        typeCheckDeclaration(decl, ctx).map(tpe => ctx.addName(decl.name, tpe))
      }
      _ <- block.statements.traverse(stmt => typeCheckStatement(stmt, ctx2))
    } yield ()

  // TODO: this only works for block-level declarations, not functions?
  def typeCheckDeclaration(decl: Declaration, ctx: TypingContext): Either[String, Type] =
    for {
      _ <- ctx.validType(decl.tpe)
      _ <- decl.initializer match {
        case Some(Initializer.Expression(expr)) =>
          typeCheckExpression(expr, ctx).flatMap(tpe =>
            if (tpe == decl.tpe) Right(())
            else Left("declaration types and initializer type are not equal")
          )
        // TODO: Initializers
        case _ => Right(())
      }
    } yield decl.tpe

  def typeCheckStatement(stmt: Statement, ctx: TypingContext): Either[String, Unit] =
    stmt match {
      case Statement.Compound(block) =>
        typeCheckBlock(block, ctx)
      case Statement.Expression(expr) =>
        expr.expr match {
          case Some(e) => typeCheckExpression(e, ctx).void
          case None    => Right(())
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
            } yield ()
        }
      case Statement.Jump(jump) =>
        jump match {
          case JumpStatement.Return(ret) =>
            // TODO: match return type of function with type here
            ret match {
              case Some(expr) => typeCheckExpression(expr, ctx).void
              case None       => Right(())
            }
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
        // $3.2.2.1: the type of an lvalue expression is the unqualified version of the type of the lvalue
        ctx
          .getName(ident)
          .fold(Left(s"identifier $ident not found"))(Right(_))
          .map(_.unqualified)
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
          paramTypes <- args.traverse(expr => typeCheckExpression(expr, ctx))
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
      case Expression.Cast(castTpe, expr) =>
        // TODO: these rules must be enriched. e.g. only arbitrary integers may be converted to pointers
        for {
          _ <-
            if (castTpe.unqualified.isScalar) Right(())
            else Left("lhs operand must have scalar type")
          tpe <- typeCheckExpression(expr, ctx)
          _ <- if (tpe.isScalar) Right(()) else Left("rhs operand must have scalar type")
        } yield castTpe
      case x => Left(s"invalid expression $x")
    }

    expr.tpe = tpe.toOption

    tpe
  }

  def isLvalue(expr: Expression): Boolean =
    expr match {
      case Expression.Identifier(_) => true
      case _                        => false
    }

}
