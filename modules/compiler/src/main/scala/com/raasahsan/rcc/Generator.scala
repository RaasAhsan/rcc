package com.raasahsan.rcc

object Generator {

  import Assembly._
  import AST._

  def generate(unit: TranslationUnit): Int = {

    ???
  }

  def findDeclarations(statement: Statement): List[Declaration] =
    statement match {
      case Statement.Compound(compound) =>
        val decls = compound.declarationList.map(_.declarations.toList).getOrElse(Nil)
        val rest =
          compound.statementList.map(_.statements.toList).getOrElse(Nil).flatMap(findDeclarations)
        decls ++ rest
      case _ => Nil
    }

  private def functionName(fd: FunctionDefinition): Option[Identifier] =
    fd.declarator.directDeclarator match {
      case DirectDeclarator.FunctionDeclarator(decl, _) =>
        decl match {
          case DirectDeclarator.Identifier(name) => Some(name)
          case _                                 => None
        }
      case DirectDeclarator.Identifiers(decl, _) =>
        decl match {
          case DirectDeclarator.Identifier(name) => Some(name)
          case _                                 => None
        }
      case _ => None
    }

  def generateFunctionDefinition(fd: FunctionDefinition): Program = {
    val declarations = findDeclarations(Statement.Compound(fd.statements))
    val name = functionName(fd).get

    val lines = List(
      Label(name.value).line,
      Instruction.Push(Register.rbp.operand).line,
      Instruction.Mov(Register.rbp.operand, Register.rsp.operand).line,
      Instruction.Pop(Register.rbp.operand).line,
      Instruction.Return.line
    )

    Program(lines)
  }

  val runtime = s"""
  global _start

  section .text
  """

}
