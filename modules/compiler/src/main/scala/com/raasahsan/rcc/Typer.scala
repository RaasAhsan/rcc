package com.raasahsan.rcc

import cats.syntax.all._

// TODO: only typing semantics or other semantics here? like unique declarations
object Typer {

  import IR._

  type TypingContext = Map[Identifier, Type]

  def typeCheck(m: Module): Either[String, TypingContext] =
    m.moduleDeclarations.toList
      .foldLeftM(Map[Identifier, Type]()) { case (ctx, ed) =>
        ed match {
          case ModuleDeclaration.FunctionDefinition(fd) =>
            typeCheckFunctionDefinition(fd, ctx).map(tpe => ctx + (fd.name -> tpe))
          case ModuleDeclaration.Declaration(decl) =>
            typeCheckDeclaration(decl, ctx).map(tpe => ctx + (decl.name -> tpe))
          case x => throw new RuntimeException(s"not implemented for $x")
        }
      }

  def typeCheckFunctionDefinition(
      fd: FunctionDefinition,
      ctx: Map[Identifier, Type]
  ): Either[String, Type] =
    for {
      params <- fd.parameters.fold(Left("no function parameters"))(Right(_))
      paramTpes = params.map { p => p.name -> p.tpe }
      _ <- typeCheckBlock(fd.block, ctx ++ paramTpes.toMap)
    } yield {
      val tpe = Type.Function(paramTpes.map(_._2), fd.returnTpe)
      fd.tpe = Some(tpe)
      tpe
    }

  def typeCheckBlock(block: Block, ctx: Map[Identifier, Type]): Either[String, Unit] =
    for {
      ctx2 <- block.declarations.foldLeftM(ctx) { (ctx, decl) =>
        typeCheckDeclaration(decl, ctx).map(tpe => ctx + (decl.name -> tpe))
      }
      _ <- block.statements.traverse(stmt => typeCheckStatement(stmt, ctx2))
    } yield ()

  // TODO: this only works for block-level declarations, not functions?
  def typeCheckDeclaration(decl: Declaration, ctx: TypingContext): Either[String, Type] =
    for {
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

  def typeNameToType(typeName: TypeName): Option[Type] = {
    val key = typeName.specifierQualifiers.toList.collect {
      case TypeSpecifierOrQualifier.Specifier(s) => s
    }.toSet
    specifierMapping.get(key).map(b => typeName.abstractDeclarator.fold(b)(_ => Type.Pointer(b)))
  }

}
