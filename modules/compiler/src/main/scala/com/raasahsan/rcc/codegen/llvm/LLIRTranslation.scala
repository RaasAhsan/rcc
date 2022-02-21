package com.raasahsan.rcc.codegen.llvm

import com.raasahsan.rcc.IR

import cats.syntax.all._

object LLIRTranslation {

  // For variables, value represents a pointer to a memory location representing tpe
  final case class IRSymbol(identifier: IR.Identifier, value: LLIR.Value, tpe: LLIR.Type)

  final case class ExpressionGen(code: List[LLIR.Instruction], value: LLIR.Value, tpe: LLIR.Type)

  def translate(module: IR.Module): LLIR.Module = {
    val topLevelDecls = module.moduleDeclarations.toList.collect {
      case IR.ModuleDeclaration.FunctionDefinition(fd) =>
        LLIR.TopLevelDeclaration.FunctionDefinition(translateFunctionDefinition(fd))
    }

    LLIR.Module(topLevelDecls)
  }

  def translateFunctionDefinition(fd: IR.FunctionDefinition): LLIR.FunctionDefinition = {
    var local: Int = -1

    def nextLocal(): Int = {
      local += 1
      local
    }

    def translateStatement(
        stmt: IR.Statement,
        symbols: Map[IR.Identifier, IRSymbol]
    ): List[LLIR.Instruction] =
      stmt match {
        case IR.Statement.Expression(expr) =>
          expr.expr.map(e => translateExpression(e, symbols).code).getOrElse(Nil)
        case IR.Statement.Compound(block) =>
          translateBlock(block, symbols)
        case IR.Statement.Jump(jump) =>
          jump match {
            case IR.JumpStatement.Return(ret) =>
              ret match {
                case Some(e) =>
                  val gen = translateExpression(e, symbols)
                  gen.code ++ List(LLIR.Op.Ret(LLIR.Return.Value(gen.tpe, gen.value)).instruction)
                case None => List(LLIR.Op.Ret(LLIR.Return.Void).instruction)
              }
            case x => throw new RuntimeException(s"not implemented for $x")
          }
        case x => throw new RuntimeException(s"not implemented for $x")
      }

    def translateBlock(
        stmt: IR.Block,
        symbols: Map[IR.Identifier, IRSymbol]
    ): List[LLIR.Instruction] = {
      final case class DeclarationState(
          instructions: List[LLIR.Instruction],
          symbols: Map[IR.Identifier, IRSymbol]
      ) {
        def addInstructions(ni: List[LLIR.Instruction]): DeclarationState =
          DeclarationState(instructions ++ ni, symbols)
        def addSymbol(ident: IR.Identifier, symbol: IRSymbol): DeclarationState =
          DeclarationState(instructions, symbols + (ident -> symbol))
      }
      object DeclarationState {
        def apply(): DeclarationState = DeclarationState(Nil, Map())
      }

      // TODO: allocate all space for function at the same time (in all blocks)

      val declState = stmt.declarations
        .foldLeft(DeclarationState()) { case (acc, decl) =>
          val ident = decl.name

          // TODO: fix alignment
          val localIndex = nextLocal()
          val localValue = LLIR.Value.Local(localIndex)
          val localTpe = translateType(decl.tpe)
          val alloc =
            LLIR.Op.Alloca(localTpe, Some(typeAlignment(localTpe))).instruction(localIndex)

          val exprGen = decl.initializer.map {
            case IR.Initializer.Expression(expr) =>
              val gen = translateExpression(expr, acc.symbols)
              val store = LLIR.Op
                .Store(
                  false,
                  gen.tpe,
                  gen.value,
                  LLIR.Type.Pointer(gen.tpe),
                  localValue,
                  Some(typeAlignment(gen.tpe))
                )
                .instruction
              gen.code ++ List(store)
            case _ => ???
          }
          val symbol = IRSymbol(ident, localValue, localTpe)
          acc.addInstructions(List(alloc) ++ exprGen.getOrElse(Nil)).addSymbol(ident, symbol)
        }

      val body = stmt.statements
        .map(stmt => translateStatement(stmt, declState.symbols))
        .combineAll

      declState.instructions ++ body
    }

    def translateExpression(
        expr: IR.Expression,
        symbols: Map[IR.Identifier, IRSymbol]
    ): ExpressionGen = {
      val exprTpe = translateType(expr.tpe.get)
      expr match {
        case IR.Expression.Constant(c) =>
          c match {
            case IR.Constant.IntegerConstant(i) =>
              ExpressionGen(Nil, LLIR.Value.Integer(i), exprTpe)
          }
        case IR.Expression.Identifier(ident) =>
          val entry = symbols.get(ident).get
          val index = nextLocal()
          val load = LLIR.Op
            .Load(
              false,
              entry.tpe,
              LLIR.Type.Pointer(entry.tpe),
              entry.value,
              Some(typeAlignment(entry.tpe))
            )
            .instruction(index)
          ExpressionGen(List(load), LLIR.Value.Local(index), entry.tpe)
        case IR.Expression.Plus(e1, e2) =>
          val gen1 = translateExpression(e1, symbols)
          val gen2 = translateExpression(e2, symbols)
          val resultLocal = nextLocal()
          val add = LLIR.Op.Add(exprTpe, gen1.value, gen2.value).instruction(resultLocal)
          ExpressionGen(gen1.code ++ gen2.code ++ List(add), LLIR.Value.Local(resultLocal), exprTpe)
        // TODO: is it possible to express this as a compositional fold?
        // Note how a variable reference loads from the memory location, whereas a pointer just returns the value
        case IR.Expression.Reference(rexpr) =>
          rexpr match {
            case IR.Expression.Identifier(ident) =>
              val symbol = symbols.get(ident).get
              ExpressionGen(Nil, symbol.value, LLIR.Type.Pointer(symbol.tpe))
            case x => throw new RuntimeException(s"not implemented for $x")
          }
        // TODO: make this more compositional
        // TODO: this doesn't work on *&var
        case IR.Expression.Dereference(rexpr) =>
          rexpr match {
            case IR.Expression.Identifier(ident) =>
              val symbol = symbols.get(ident).get
              // TODO: this needs to be consistent with symbol table
              val ptrTpe = translateType(rexpr.tpe.get)
              val derefTpe = translateType(expr.tpe.get)
              val ptrLocal = nextLocal()
              val derefLocal = nextLocal()
              val l1 = LLIR.Op
                .Load(
                  false,
                  ptrTpe,
                  LLIR.Type.Pointer(ptrTpe),
                  symbol.value,
                  Some(typeAlignment(ptrTpe))
                )
                .instruction(ptrLocal)
              val l2 = LLIR.Op
                .Load(
                  false,
                  derefTpe,
                  LLIR.Type.Pointer(derefTpe),
                  LLIR.Value.Local(ptrLocal),
                  Some(typeAlignment(derefTpe))
                )
                .instruction(derefLocal)
              ExpressionGen(List(l1, l2), LLIR.Value.Local(derefLocal), derefTpe)
            case x => throw new RuntimeException(s"not implemented for $x")
          }
        case x => throw new RuntimeException(s"not implemented for $x")
      }
    }

    val irArguments = fd.parameters.get.map { p =>
      LLIR.FunctionArgument(translateType(p.tpe), Nil, Some(p.name.value))
    }
    val startingSymbols = fd.parameters.get.map { p =>
      val index = nextLocal()
      p.name -> IRSymbol(p.name, LLIR.Value.Local(index), translateType(p.tpe))
    }.toMap
    // arguments begin with %0, registers begin with %1
    if (local == -1) {
      local = 0
    }
    val instructions = translateBlock(fd.block, startingSymbols)

    LLIR.FunctionDefinition(
      None,
      None,
      translateType(fd.returnTpe),
      fd.name.value,
      irArguments,
      instructions
    )
  }

  def translateType(tpe: IR.Type): LLIR.Type =
    tpe match {
      case IR.Type.Int          => LLIR.Type.Integer(32)
      case IR.Type.Pointer(tpe) => LLIR.Type.Pointer(translateType(tpe))
      case _                    => ???
    }

  // TODO: depends on target/data layout? triples arm-none-eabi, x86_64-pc-linux-gnu
  def typeAlignment(tpe: LLIR.Type): Int =
    tpe match {
      case LLIR.Type.Integer(bits) => bits / 8
      case LLIR.Type.Pointer(_)    => 8
      case _                       => ???
    }

}
