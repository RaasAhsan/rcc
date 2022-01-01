package com.raasahsan.rcc

import scala.collection.mutable
import cats.syntax.all._

class Generator {

  import Assembly._
  import AST._

  // TODO: typed registers will be necessary
  val CallParameterRegisters =
    List(Register.edi, Register.esi, Register.edx, Register.ecx, Register.e8, Register.e9)

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

    val arguments = fd.declarator.functionParameters
      .map(_.map(_._1))
      .get
      .zip(CallParameterRegisters)
      .map((ident, reg) => ident -> RegisterAssignment.Register(reg))
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
                  val storage = frame.allocateNamed(ident.value, 4)
                  initDecl.initializer
                    .map {
                      case Initializer.Expression(expr) =>
                        val (gen, assign) = generateExpression(expr)
                        instructions(
                          gen,
                          load(storage, assign)
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
                .map((gen, assign) => gen |+| loadIntoRegister(Register.eax, assign))
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
              frame.get(ident.value)
          })
        case Expression.StringLiteral(literal) =>
          val label = addStringLiteral(literal)
          val alloc = frame.allocate(8) // TODO: pointer size constant
          instructions(
            load(alloc, RegisterAssignment.Label(label))
          ) -> alloc
        case Expression.Assignment(lhs, rhs) =>
          // TODO: Assert LHS is a modifiable lvalue (identifier or array index) in the type system
          val (genL, assignL) = generateExpression(lhs)
          val (genR, assignR) = generateExpression(rhs)
          instructions(
            genL,
            genR,
            load(assignL, assignR)
          ) -> assignL
        case Expression.Plus(lhs, rhs) =>
          // TODO: helper function to load assigned register into somewhere
          val (genL, assignL) = generateExpression(lhs)
          val (genR, assignR) = generateExpression(rhs)
          val resultAlloc = frame.allocate(4)
          instructions(
            genL,
            genR,
            loadIntoRegister(Register.eax, assignL),
            loadIntoRegister(Register.edx, assignR),
            Instruction.Add(Register.eax.operand, Register.edx.operand),
            load(resultAlloc, RegisterAssignment.Register(Register.eax))
          ) -> resultAlloc
        case Expression.FunctionCall(fexpr, fargs) =>
          fexpr match {
            case Expression.Identifier(fname) =>
              val args = fargs.map(_.args.toList).getOrElse(Nil)
              val returnAlloc = frame.allocate(4)

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
                Instruction.Call(fname.value),
                load(returnAlloc, RegisterAssignment.Register(Register.eax))
              ) -> returnAlloc
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
          loadIntoAddress(addr, source)
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
            Instruction.Mov(Register.eax.operand, assignAddr.operand),
            Instruction.Mov(addr.operand, Register.eax.operand)
          )
        case RegisterAssignment.Register(srcReg) =>
          instructions(
            Instruction.Mov(addr.operand, srcReg.operand)
          )
        case RegisterAssignment.Label(label) =>
          instructions(
            Instruction.Mov(addr.operand, label.operand)
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
        case _ => ???
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
