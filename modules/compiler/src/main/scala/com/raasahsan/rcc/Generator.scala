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
        val rest = compound.statementList.map(_.statements.toList).getOrElse(Nil).flatMap(findDeclarations)
        decls ++ rest
      case _ => Nil
    }

  def generateFunctionDefinition(fd: FunctionDefinition): Unit = {
    val declarations = findDeclarations(Statement.Compound(fd.statements))

  }

  val runtime = s"""
  global _start

  section .text
  """

}
