package com.raasahsan.rcc

import cats.data.{Kleisli, EitherNel}

object Typer2 {

  import TypedAST._

  type Context = Map[Identifier, Type]
  type TypeCheckResult[F[_]] = Kleisli[[X] =>> EitherNel[Throwable, X], Context, F[Type]]

  trait TypeCheck[F[_]] {
    def check(in: F[Unit]): TypeCheckResult[F]
  }

  implicit val typeCheckForTranslationUnit: TypeCheck[TranslationUnit] =
    new TypeCheck[TranslationUnit] {
      override def check(in: TranslationUnit[Unit]): TypeCheckResult[TranslationUnit] = {
        ???
      }
    }

  implicit val typeCheckForExpr: TypeCheck[Expr] = new TypeCheck[Expr] {
    override def check(in: Expr[Unit]): TypeCheckResult[Expr] = Kleisli { _ =>
      in.next match {
        case Expression.Constant(c) =>
          c match {
            case Constant.IntegerConstant(_) => Right(Cofree(Type.Int, Expression.Constant(c)))
            case _                           => ???
          }
        case _ => ???
      }
    }
  }

}
