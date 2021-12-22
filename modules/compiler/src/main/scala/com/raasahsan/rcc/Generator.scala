package com.raasahsan.rcc

import scala.collection.mutable

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

  private def directDeclaratorIdentifier(dd: DirectDeclarator): Option[Identifier] =
    dd match {
      case DirectDeclarator.Identifier(id) => Some(id)
      case _ => None
    }

  def generateFunctionDefinition(fd: FunctionDefinition): Program = {
    val name = functionName(fd).get
    
    // Perform register allocation and code generation at the same time
    // Three-address code makes this much simpler, and perhaps makes
    // it easier to decouple.
    // TODO: RegisterAllocator class?

    // TODO: Offset newtype

    val symbols = new mutable.HashMap[String, Int]
    var offset = 0

    def nextOffset(size: Int): Int =
      offset += size
      offset

    def allocateOrGet(name: String, size: Int): Int =
      symbols.get(name) match {
        case Some(offset) => offset
        case None =>
          val next = nextOffset(size)
          symbols.put(name, next)
          next
      }

    // Allocate storage on the stack for all local variables, including nested compounds
    val declarations = findDeclarations(Statement.Compound(fd.statements))
    val names = declarations.flatMap { decl =>
      decl.initDeclaratorList.get.declarators.toList.map { initDecl =>
        // TODO: handle pointers, typing here
        directDeclaratorIdentifier(initDecl.declarator.directDeclarator).get
      }
    }

    names.foreach { ident =>
      // handle pointers, typing here
      allocateOrGet(ident.value, 4)  
    }

    // Next, evaluate all initializers and statements, allocating storage on stack if necessary
    def generateStatement(statement: Statement): Unit = {
      
    }
    
    // TODO: Boilerplate for lines
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
