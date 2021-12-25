package com.raasahsan.rcc

import scala.collection.mutable
import cats.syntax.all._

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
      case _                               => None
    }

  def generateFunctionDefinition(fd: FunctionDefinition): Program = {
    val name = functionName(fd).get

    // Perform register allocation and code generation at the same time
    // Three-address code makes this much simpler, and perhaps makes
    // it easier to decouple.
    // TODO: RegisterAllocator class?

    // TODO: Offset newtype

    // TODO: typed registers will be necessary
    val callParameterRegisters =
      List(Register.edi, Register.esi, Register.edx, Register.ecx, Register.e8, Register.e9)

    enum RegisterAssignment {
      case Memory(address: Address)
      case Register(reg: Assembly.Register)
      case Constant(value: Int)
    }

    val arguments = (fd.declarator.directDeclarator match {
      case DirectDeclarator.FunctionDeclarator(_, params) =>
        params.parameterList.parameters.toList.map {
          case ParameterDeclaration.Declarator(_, decl) =>
            directDeclaratorIdentifier(decl.directDeclarator).get
        }
      case DirectDeclarator.Identifiers(_, None) => Nil
      case _                                     => ???
    }).zip(callParameterRegisters)
      .map((ident, reg) => ident -> RegisterAssignment.Register(reg))
      .toMap

    val symbols = new mutable.HashMap[String, Int]
    var stackOffset = 0

    def nextStackOffset(size: Int): Int =
      stackOffset += size
      stackOffset

    def allocateOrGet(name: String, size: Int): Int =
      symbols.get(name) match {
        case Some(offset) => offset
        case None =>
          val next = nextStackOffset(size)
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

    def postamble = List(
      Instruction.Mov(Register.rsp.operand, Register.rbp.operand).line,
      Instruction.Pop(Register.rbp.operand).line,
      Instruction.Ret.line
    )

    // Next, evaluate all initializers and statements, allocating storage on stack if necessary
    // Needs a register/stack allocator for subexpressions
    def generateStatement(statement: Statement): List[Line] =
      statement match {
        case Statement.Compound(stmt) =>
          val genDecls =
            stmt.declarationList.map(_.declarations.toList).getOrElse(Nil).flatMap { decl =>
              decl.initDeclaratorList.get.declarators.toList.flatMap { initDecl =>
                val ident = directDeclaratorIdentifier(initDecl.declarator.directDeclarator).get
                // TODO: we shouldn't be allocating at this point
                val identOffset = allocateOrGet(ident.value, 4)
                val address = Address.IndirectDisplacement(Register.rbp, -identOffset)
                initDecl.initializer
                  .map { init =>
                    init match {
                      case Initializer.Expression(expr) =>
                        val (gen, assign) = generateExpression(expr)
                        gen ++ loadIntoRegister(Register.eax, assign) ++ List(
                          Instruction.Mov(address.operand, Register.eax.operand).line
                        )
                      case _ => ???
                    }
                  }
                  .getOrElse(Nil)
              }
            }

          val genStmts =
            stmt.statementList.map(_.statements.toList).getOrElse(Nil).flatMap(generateStatement)

          genDecls ++ genStmts
        case Statement.Expression(stmt) =>
          stmt.expr.fold(List(Instruction.Nop.line))(e => generateExpression(e)._1)
        case Statement.Jump(jump) =>
          jump match {
            case JumpStatement.Return(expr) =>
              val gen = expr
                .map(generateExpression)
                .map((gen, assign) => gen ++ loadIntoRegister(Register.eax, assign))
                .getOrElse(Nil)
              gen ++ postamble
            case _ => ???
          }
        case _ => ???
      }

    // Generate code for evaluating the expression, and the offset
    // where the result is stored in memory.
    def generateExpression(expr: Expression): (List[Line], RegisterAssignment) =
      expr match {
        case Expression.Constant(const) =>
          const match {
            case Constant.IntegerConstant(value) => (Nil, RegisterAssignment.Constant(value))
            case _                               => ???
          }
        case Expression.Identifier(ident) =>
          // TODO: this should have already been allocated
          Nil -> (arguments.get(ident) match {
            case Some(assign) =>
              assign
            case None =>
              val next = allocateOrGet(ident.value, 4)
              RegisterAssignment.Memory(Address.IndirectDisplacement(Register.rbp, -next))
          })
        case Expression.Assignment(lhs, rhs) =>
          // TODO: Assert LHS is an l-value (identifier or array index) in the type system
          val (genL, assignL) = generateExpression(lhs)
          assignL match {
            case RegisterAssignment.Memory(address) =>
              val (genR, assignR) = generateExpression(rhs)
              val gen = loadIntoAddress(address, assignR)
              (genL ++ genR ++ gen) -> assignL
            case RegisterAssignment.Constant(_) => throw new RuntimeException("invalid lvalue")
            case RegisterAssignment.Register(_) => throw new RuntimeException("unsupported")
          }
        case Expression.Plus(lhs, rhs) =>
          // TODO: helper function to load assigned register into somewhere
          val (genL, assignL) = generateExpression(lhs)
          val (genR, assignR) = generateExpression(rhs)
          val addr = Address.IndirectDisplacement(Register.rbp, -nextStackOffset(4))
          val gen = genL ++ genR ++ loadIntoRegister(Register.eax, assignL) ++ loadIntoRegister(
            Register.edx,
            assignR
          ) ++ List(
            Instruction.Add(Register.eax.operand, Register.edx.operand).line,
            Instruction.Mov(addr.operand, Register.eax.operand).line
          )
          gen -> RegisterAssignment.Memory(addr)
        case Expression.FunctionCall(fexpr, fargs) =>
          fexpr match {
            case Expression.Identifier(fname) =>
              val args = fargs.map(_.args.toList).getOrElse(Nil)
              val returnAddr = Address.IndirectDisplacement(Register.rbp, -nextStackOffset(4))

              // TODO: more than 6 args will require changes to how we calculate stack offsets
              // and perform 16-byte rsp alignment
              if (args.length > 6) {
                throw new RuntimeException("more than 6 arguments")
              }
              val genExprs = callParameterRegisters
                .zip(args)
                .map { (register, expr) =>
                  val (genExpr, assignExpr) = generateExpression(expr)
                  genExpr ++ loadIntoRegister(register, assignExpr)
                }
                .combineAll

              // TODO: what do we do for void functions?
              val gen = genExprs ++ List(
                Instruction.Call(fname.value).line,
                Instruction.Mov(returnAddr.operand, Register.eax.operand).line
              )
              gen -> RegisterAssignment.Memory(returnAddr)
            case _ => ???
          }
        case _ => ???
      }

    def loadIntoAddress(addr: Address, assign: RegisterAssignment): List[Line] =
      assign match {
        case RegisterAssignment.Constant(c) =>
          List(
            Instruction.Mov(addr.operand, Immediate(c).operand).line
          )
        case RegisterAssignment.Memory(assignAddr) =>
          List(
            Instruction.Mov(Register.eax.operand, assignAddr.operand).line,
            Instruction.Mov(addr.operand, Register.eax.operand).line
          )
        case RegisterAssignment.Register(srcReg) =>
          List(
            Instruction.Mov(addr.operand, srcReg.operand).line
          )
      }

    def loadIntoRegister(reg: Register, assign: RegisterAssignment): List[Line] =
      assign match {
        case RegisterAssignment.Constant(c) =>
          List(
            Instruction.Mov(reg.operand, Immediate(c).operand).line
          )
        case RegisterAssignment.Memory(addr) =>
          List(
            Instruction.Mov(reg.operand, addr.operand).line
          )
        case RegisterAssignment.Register(srcReg) =>
          List(
            Instruction.Mov(reg.operand, srcReg.operand).line
          )
      }

    val gen = generateStatement(Statement.Compound(fd.statements))

    // TODO: rsp is 64-bit but we're using 32-bit values for now
    val alignedStackOffset = ((stackOffset + 8) & 0xfffffff0) + 8

    // TODO: maybe we can label it and jump?
    val preamble = List(
      Label(name.value).line,
      Instruction.Push(Register.rbp.operand).line, // push 8 bytes
      Instruction.Mov(Register.rbp.operand, Register.rsp.operand).line,
      Instruction.Sub(Register.rsp.operand, Operand.Immediate(Immediate(alignedStackOffset))).line
    )

    // TODO: the postamble here can be dead code
    // Type system can ensure a return statement is done for typed functions
    // for Void we may have to insert one at the end
    Program(preamble ++ gen ++ postamble)
  }

  // TODO: we need to generate code to access the appropriate l-value. there are different syntactic forms
  enum Lvalue {
    case Identifier(name: String)
  }

}
