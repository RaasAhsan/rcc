package com.raasahsan.rcc

import scala.collection.mutable
import cats.syntax.all._

object Generator {

  import Assembly._
  import AST._

  // TODO: typed registers will be necessary
  val CallParameterRegisters =
    List(Register.edi, Register.esi, Register.edx, Register.ecx, Register.e8, Register.e9)

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

  def generateFunctionDefinition(fd: FunctionDefinition): Lines = {
    val name = functionName(fd).get

    // Perform register allocation and code generation at the same time
    // Three-address code makes this much simpler, and perhaps makes
    // it easier to decouple.
    // TODO: RegisterAllocator class?

    // TODO: Offset newtype

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
    }).zip(CallParameterRegisters)
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

    def postamble = instructions(
      Instruction.Mov(Register.rsp.operand, Register.rbp.operand).line,
      Instruction.Pop(Register.rbp.operand).line,
      Instruction.Ret.line
    )

    // Next, evaluate all initializers and statements, allocating storage on stack if necessary
    // Needs a register/stack allocator for subexpressions
    def generateStatement(statement: Statement): Lines =
      statement match {
        case Statement.Compound(stmt) =>
          val genDecls =
            stmt.declarationList
              .map(_.declarations.toList)
              .getOrElse(Nil)
              .flatMap { decl =>
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
                          instructions(
                            gen,
                            loadIntoRegister(Register.eax, assign),
                            Instruction.Mov(address.operand, Register.eax.operand).line
                          )
                        case _ => ???
                      }
                    }
                }
              }
              .combineAll

          val genStmts =
            stmt.statementList
              .map(_.statements.toList)
              .getOrElse(Nil)
              .map(generateStatement)
              .combineAll

          genDecls |+| genStmts
        case Statement.Expression(stmt) =>
          stmt.expr.fold(instructions(Instruction.Nop.line))(e => generateExpression(e)._1)
        case Statement.Jump(jump) =>
          jump match {
            case JumpStatement.Return(expr) =>
              val gen = expr
                .map(generateExpression)
                .map((gen, assign) => gen |+| loadIntoRegister(Register.eax, assign))
                .getOrElse(Lines.empty)
              gen |+| postamble
            case _ => ???
          }
        case _ => ???
      }

    // Generate code for evaluating the expression, and the offset
    // where the result is stored in memory.
    def generateExpression(expr: Expression): (Lines, RegisterAssignment) =
      expr match {
        case Expression.Constant(const) =>
          const match {
            case Constant.IntegerConstant(value) =>
              (Lines.empty, RegisterAssignment.Constant(value))
            case _ => ???
          }
        case Expression.Identifier(ident) =>
          // TODO: this should have already been allocated
          Lines.empty -> (arguments.get(ident) match {
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
              instructions(genL, genR, gen) -> assignL
            case RegisterAssignment.Constant(_) => throw new RuntimeException("invalid lvalue")
            case RegisterAssignment.Register(_) => throw new RuntimeException("unsupported")
          }
        case Expression.Plus(lhs, rhs) =>
          // TODO: helper function to load assigned register into somewhere
          val (genL, assignL) = generateExpression(lhs)
          val (genR, assignR) = generateExpression(rhs)
          val addr = Address.IndirectDisplacement(Register.rbp, -nextStackOffset(4))
          instructions(
            genL,
            genR,
            loadIntoRegister(Register.eax, assignL),
            loadIntoRegister(Register.edx, assignR),
            Instruction.Add(Register.eax.operand, Register.edx.operand).line,
            Instruction.Mov(addr.operand, Register.eax.operand).line
          ) -> RegisterAssignment.Memory(addr)
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
              val genExprs = CallParameterRegisters
                .zip(args)
                .map { (register, expr) =>
                  val (genExpr, assignExpr) = generateExpression(expr)
                  genExpr |+| loadIntoRegister(register, assignExpr)
                }
                .combineAll

              // TODO: what do we do for void functions?
              instructions(
                genExprs,
                Instruction.Call(fname.value).line,
                Instruction.Mov(returnAddr.operand, Register.eax.operand).line
              ) -> RegisterAssignment.Memory(returnAddr)
            case _ => ???
          }
        case _ => ???
      }

    def loadIntoAddress(addr: Address, assign: RegisterAssignment): Lines =
      assign match {
        case RegisterAssignment.Constant(c) =>
          instructions(
            Instruction.Mov(addr.operand, Immediate(c).operand).line
          )
        case RegisterAssignment.Memory(assignAddr) =>
          instructions(
            Instruction.Mov(Register.eax.operand, assignAddr.operand).line,
            Instruction.Mov(addr.operand, Register.eax.operand).line
          )
        case RegisterAssignment.Register(srcReg) =>
          instructions(
            Instruction.Mov(addr.operand, srcReg.operand).line
          )
      }

    def loadIntoRegister(reg: Register, assign: RegisterAssignment): Lines =
      assign match {
        case RegisterAssignment.Constant(c) =>
          instructions(
            Instruction.Mov(reg.operand, Immediate(c).operand).line
          )
        case RegisterAssignment.Memory(addr) =>
          instructions(
            Instruction.Mov(reg.operand, addr.operand).line
          )
        case RegisterAssignment.Register(srcReg) =>
          instructions(
            Instruction.Mov(reg.operand, srcReg.operand).line
          )
      }

    val gen = generateStatement(Statement.Compound(fd.statements))

    // TODO: rsp is 64-bit but we're using 32-bit values for now
    val alignedStackOffset = ((stackOffset + 8) & 0xfffffff0) + 8

    // TODO: maybe we can label it and jump?
    val preamble = instructions(
      Directive.Global(name.value).line,
      Label(name.value).line,
      Instruction.Push(Register.rbp.operand).line, // push 8 bytes
      Instruction.Mov(Register.rbp.operand, Register.rsp.operand).line,
      Instruction.Sub(Register.rsp.operand, Operand.Immediate(Immediate(alignedStackOffset))).line
    )

    // TODO: the postamble here can be dead code
    // Type system can ensure a return statement is done for typed functions
    // for Void we may have to insert one at the end
    instructions(
      preamble,
      gen,
      postamble
    )
  }

  def generateTranslationUnit(unit: TranslationUnit): Lines = {
    // TODO: revise with correct external/internal linkage semantics
    val genFunctions = unit.externalDeclarations.toList.map {
      case ExternalDeclaration.FunctionDefinition(fd) => generateFunctionDefinition(fd)
      case _                                          => Lines.empty
    }.combineAll

    instructions(
      Directive.IntelSyntax.line,
      Directive.Text.line,
      genFunctions
    )
  }

  // TODO: we need to generate code to access the appropriate l-value. there are different syntactic forms
  enum Lvalue {
    case Identifier(name: String)
  }

}
