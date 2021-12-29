package com.raasahsan.rcc

// TODO: only typing semantics or other semantics here? like unique declarations
object Typer {

  import AST._

  def typeCheckFunctionDefinition(fd: FunctionDefinition, ctx: Map[Identifier, Type]): Option[Type] = {


    None
  }

  def typeCheckStatement(stmt: Statement): Boolean =
    stmt match {
      case Statement.Compound(compound) => 
        // compound.declarationList.map(_.declarations.toList).getOrElse(Nil).foldLeft(ctx) { }
        ???
      case _ => ???
    }

  def typeCheck(expr: Expression, ctx: Map[Identifier, Type]): Option[Type] =
    expr match {
      case Expression.Constant(const) =>
        const match {
          case Constant.IntegerConstant(_) => Some(Type.Int)
        }
      case Expression.StringLiteral(_) => Some(Type.Pointer(Type.Char))
      case Expression.Plus(l, r) =>
        for {
          lt <- typeCheck(l, ctx)
          rt <- typeCheck(r, ctx)
          // TODO: + is valid for all arithmetic types
          _ <- if (lt == rt && rt == Type.Int) Some(()) else None
        } yield lt
      case Expression.Identifier(ident) =>
        ctx.get(ident)
      case Expression.Assignment(l, r) =>
        for {
          lt <- typeCheck(l, ctx)
          rt <- typeCheck(r, ctx)
          _ <- if (lt == rt) Some(()) else None
        } yield lt
      case _ => None
    }

}
