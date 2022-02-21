package com.raasahsan.rcc

import munit.FunSuite
import cats.data.NonEmptyList

class TyperSuite extends FunSuite {

  import IR._
  import Typer._

  test("integers") {
    assertEquals(
      typeCheckExpression(Expression.Constant(Constant.IntegerConstant(3)), Map()),
      Right(Type.Int)
    )
  }

  test("string literals") {
    assertEquals(
      typeCheckExpression(Expression.StringLiteral("hello"), Map()),
      Right(Type.Pointer(Type.Char))
    )
  }

  test("arithmetic") {
    val expr = Expression.Plus(
      Expression.Constant(Constant.IntegerConstant(3)),
      Expression.Times(
        Expression.Constant(Constant.IntegerConstant(3)),
        Expression.Minus(
          Expression.Constant(Constant.IntegerConstant(3)),
          Expression.Divide(
            Expression.Constant(Constant.IntegerConstant(10)),
            Expression.Constant(Constant.IntegerConstant(2))
          )
        )
      )
    )
    assertEquals(
      typeCheckExpression(expr, Map()),
      Right(Type.Int)
    )
  }

  test("assignment of const-qualified variable to unqualified variable") {
    assertEquals(
      typeCheckExpression(
        Expression.Assignment(Expression.Identifier(Identifier("x")), Expression.Identifier(Identifier("y"))),
        Map(
          Identifier("x") -> Type.Int,
          Identifier("y") -> Type.Qualified(Type.Int, NonEmptyList.one(TypeQualifier.Const))
        )
      ),
      Right(Type.Int)
    )
  }
}
