package com.raasahsan.rcc

import scala.collection.mutable
import cats.syntax.all._

class Generator {

  import Assembly._
  import AST._

  // TODO: typed registers will be necessary
  // r7, r6
  val CallParameterRegisters =
    List(7, 6, 2, 1, 8, 9)

  // Labels have function scope in C but global scope in assembly
  var stringLabelIndex = 0
  var functionLabelIndex = 0
  var stringLiterals = mutable.Buffer[(Label, StringLiteral)]()

  def addStringLiteral(literal: StringLiteral): Label = {
    stringLabelIndex += 1
    val label = Label(s"LC$stringLabelIndex")
    stringLiterals += (label -> literal)
    label
  }

  def nextFunctionLabelIndex(): String = {
    functionLabelIndex += 1
    s"L$functionLabelIndex"
  }

  // TODO: ReaderT[State[S, ?], SymbolTable, R]
  def generateFunctionDefinition(fd: FunctionDefinition): Lines = {
    val name = fd.functionName.get

    // Perform register allocation and code generation at the same time
    // Three-address code makes this much simpler, and perhaps makes
    // it easier to decouple.
    // TODO: RegisterAllocator class?

    // TODO: Offset newtype
    val argTypes = fd.tpe
      .collect { case Type.Function(args, _) =>
        args
      }
      .get
      .zip(fd.functionParameters.getOrElse(Nil).map(_._1))

    val arguments = argTypes
      .zip(CallParameterRegisters)
      .map { case ((tpe, ident), reg) =>
        ident -> RegisterAssignment.Register(Register.sized(reg, tpe.dataSize).get)
      }
      .toMap

    val frame = new StackFrame

    def postamble = instructions(
      Instruction.Mov(Register.rsp.operand, Register.rbp.operand),
      Instruction.Pop(Register.rbp.operand),
      Instruction.Ret
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
                  val ident = initDecl.declarator.identifier.get
                  val size = initDecl.initializer
                    .collect { case Initializer.Expression(e) =>
                      e.tpe
                    }
                    .flatten
                    .get
                    .dataSize
                  val storage = frame.allocateNamed(ident.value, size)
                  initDecl.initializer
                    .map {
                      case Initializer.Expression(expr) =>
                        val (gen, assign) = generateExpression(expr)
                        instructions(
                          gen,
                          load(storage.assignment, assign)
                        )
                      case _ => ???
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
          stmt.expr.fold(instructions(Instruction.Nop))(e => generateExpression(e)._1)
        case Statement.Jump(jump) =>
          jump match {
            case JumpStatement.Return(expr) =>
              val gen = expr
                .map(generateExpression)
                .map { (gen, assign) =>
                  expr match {
                    case Some(e) =>
                      gen |+| loadIntoRegister(Register.sized(0, e.tpe.get.dataSize).get, assign)
                    case None => gen
                  }
                }
                .getOrElse(Lines.empty)
              gen |+| postamble
            case _ => ???
          }
        case Statement.Selection(stmt) =>
          stmt match {
            case SelectionStatement.If(condition, consequent, alternative) =>
              val (gen, assign) = generateExpression(condition)

              alternative match {
                case Some(alternativeStmt) =>
                  val alt = nextFunctionLabelIndex()
                  val end = nextFunctionLabelIndex()
                  instructions(
                    gen,
                    loadIntoRegister(Register.eax, assign),
                    Instruction.Cmp(Register.eax.operand, Immediate(0).operand),
                    Instruction.Je(alt),
                    generateStatement(consequent),
                    Instruction.Jmp(end),
                    Label(alt),
                    generateStatement(alternativeStmt),
                    Label(end)
                  )
                case None =>
                  val end = nextFunctionLabelIndex()
                  instructions(
                    gen,
                    loadIntoRegister(Register.eax, assign),
                    Instruction.Cmp(Register.eax.operand, Immediate(0).operand),
                    Instruction.Jne(end),
                    generateStatement(consequent),
                    Label(end)
                  )
              }
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
          Lines.empty -> (arguments.get(ident) match {
            case Some(assign) =>
              assign
            case None =>
              frame.get(ident.value).assignment
          })
        case Expression.StringLiteral(literal) =>
          val label = addStringLiteral(literal)
          val alloc = frame.allocate(DataSize.Qword) // TODO: pointer size constant
          // TODO: sized allocations?
          instructions(
            loadIntoRegister(Register.rax, RegisterAssignment.Label(label)),
            load(alloc.assignment, RegisterAssignment.Register(Register.rax))
          ) -> alloc.assignment
        case Expression.Assignment(lhs, rhs) =>
          // TODO: Assert LHS is a modifiable lvalue (identifier or array index) in the type system
          val (genL, assignL) = generateExpression(lhs)
          val (genR, assignR) = generateExpression(rhs)
          instructions(
            genL,
            genR,
            load(assignL, assignR)
          ) -> assignL
        case e @ Expression.Plus(lhs, rhs) =>
          // TODO: helper function to load assigned register into somewhere
          val (genL, assignL) = generateExpression(lhs)
          val (genR, assignR) = generateExpression(rhs)
          val size = e.tpe.map(_.dataSize).get
          val resultAlloc = frame.allocate(size)
          instructions(
            genL,
            genR,
            loadIntoRegister(Register.sized(0, size).get, assignL),
            loadIntoRegister(Register.sized(2, size).get, assignR),
            Instruction
              .Add(Register.sized(0, size).get.operand, Register.sized(2, size).get.operand),
            load(resultAlloc.assignment, RegisterAssignment.Register(Register.sized(0, size).get))
          ) -> resultAlloc.assignment
        case Expression.FunctionCall(fexpr, fargs) =>
          fexpr match {
            case Expression.Identifier(fname) =>
              val args = fargs.map(_.args.toList).getOrElse(Nil)
              val returnSize = fexpr.tpe.collect { case Type.Function(args, ret) =>
                ret.dataSize
              }.get
              val returnAlloc = frame.allocate(returnSize)

              // TODO: more than 6 args will require changes to how we calculate stack offsets
              // and perform 16-byte rsp alignment
              if (args.length > 6) {
                throw new RuntimeException("more than 6 arguments")
              }
              val genExprs = CallParameterRegisters
                .zip(args)
                .map { (register, expr) =>
                  val (genExpr, assignExpr) = generateExpression(expr)
                  genExpr |+| loadIntoRegister(
                    Register.sized(register, expr.tpe.get.dataSize).get,
                    assignExpr
                  )
                }
                .combineAll

              // TODO: what do we do for void functions?
              instructions(
                genExprs,
                Instruction.Call(fname.value),
                load(
                  returnAlloc.assignment,
                  RegisterAssignment.Register(Register.sized(0, returnSize).get)
                )
              ) -> returnAlloc.assignment
            case _ => ???
          }
        case Expression.Dereference(expr) =>
          val (gen, assign) = generateExpression(expr)
          ???
        case _ => ???
      }

    def load(target: RegisterAssignment, source: RegisterAssignment): Lines =
      target match {
        case RegisterAssignment.Memory(addr) =>
          loadIntoAddress(addr.address, source)
        case RegisterAssignment.Register(reg) =>
          loadIntoRegister(reg, source)
        case _ => ???
      }

    def loadIntoAddress(addr: Address, assign: RegisterAssignment): Lines =
      assign match {
        case RegisterAssignment.Constant(c) =>
          instructions(
            Instruction.Mov(addr.operand, Immediate(c).operand)
          )
        case RegisterAssignment.Memory(assignAddr) =>
          instructions(
            Instruction.Mov(Register.sized(0, assignAddr.size).get.operand, assignAddr.operand),
            Instruction.Mov(addr.operand, Register.sized(0, assignAddr.size).get.operand)
          )
        case RegisterAssignment.Register(srcReg) =>
          instructions(
            Instruction.Mov(addr.operand, srcReg.operand)
          )
        case RegisterAssignment.Label(label) =>
          instructions(
            Instruction.Lea(addr.operand, label.operand)
          )
      }

    def loadIntoRegister(reg: Register, assign: RegisterAssignment): Lines =
      assign match {
        case RegisterAssignment.Constant(c) =>
          instructions(
            Instruction.Mov(reg.operand, Immediate(c).operand)
          )
        case RegisterAssignment.Memory(addr) =>
          instructions(
            Instruction.Mov(reg.operand, addr.operand)
          )
        case RegisterAssignment.Register(srcReg) =>
          instructions(
            Instruction.Mov(reg.operand, srcReg.operand)
          )
        case RegisterAssignment.Label(label) =>
          instructions(
            Instruction.Lea(reg.operand, label.operand)
          )
      }

    val gen = generateStatement(Statement.Compound(fd.statements))

    // TODO: maybe we can label it and jump?
    val preamble = instructions(
      Directive.Global(name.value),
      Label(name.value),
      Instruction.Push(Register.rbp.operand), // push 8 bytes
      Instruction.Mov(Register.rbp.operand, Register.rsp.operand),
      Instruction.Sub(Register.rsp.operand, Operand.Immediate(Immediate(frame.alignedStackOffset)))
    )

    // TODO: the postamble here can be dead code
    // Type system can ensure a return statement is done for typed functions
    // for Void we may have to insert one at the end
    instructions(
      preamble,
      gen,
      postamble,
      Line.Empty
    )
  }

  def generateTranslationUnit(unit: TranslationUnit): Lines = {
    // TODO: revise with correct external/internal linkage semantics
    val genFunctions = unit.externalDeclarations.toList.map {
      case ExternalDeclaration.FunctionDefinition(fd) => generateFunctionDefinition(fd)
      case _                                          => Lines.empty
    }.combineAll

    val genData = stringLiterals.toList.map { case (label, literal) =>
      instructions(
        label.line,
        Directive.StringLiteral(literal.value)
      )
    }.combineAll

    instructions(
      Directive.IntelSyntax,
      Directive.Text,
      genFunctions,
      Line.Empty,
      Directive.Data,
      genData
    )
  }

  // TODO: we need to generate code to access the appropriate l-value. there are different syntactic forms
  enum Lvalue {
    case Identifier(name: String)
  }

}
