package com.raasahsan.rcc
package codegen.llvm

import cats.syntax.all._

object LLVMBackend {

  // For variables, value represents a pointer to a memory location representing tpe
  final case class IRSymbol(identifier: AST.Identifier, value: IR.Value, tpe: IR.Type)

  final case class ExpressionGen(code: List[IR.Instruction], value: IR.Value, tpe: IR.Type)

  def translate(translationUnit: AST.TranslationUnit): IR.Module = {
    val topLevelDecls = translationUnit.externalDeclarations.toList.collect {
      case AST.ExternalDeclaration.FunctionDefinition(fd) =>
        IR.TopLevelDeclaration.FunctionDefinition(translateFunctionDefinition(fd))
    }

    IR.Module(topLevelDecls)
  }

  def translateFunctionDefinition(fd: AST.FunctionDefinition): IR.FunctionDefinition = {
    var local: Int = -1

    def nextLocal(): Int = {
      local += 1
      local
    }

    def translateStatement(
        stmt: AST.Statement,
        symbols: Map[AST.Identifier, IRSymbol]
    ): List[IR.Instruction] =
      stmt match {
        case AST.Statement.Expression(expr) =>
          expr.expr.map(e => translateExpression(e, symbols).code).getOrElse(Nil)
        case AST.Statement.Compound(compound) =>
          translateCompoundStatement(compound, symbols)
        case AST.Statement.Jump(jump) =>
          jump match {
            case AST.JumpStatement.Return(ret) =>
              ret match {
                case Some(e) =>
                  val gen = translateExpression(e, symbols)
                  gen.code ++ List(IR.Op.Ret(IR.Return.Value(gen.tpe, gen.value)).instruction)
                case None => List(IR.Op.Ret(IR.Return.Void).instruction)
              }
          }
      }

    def translateCompoundStatement(
        stmt: AST.CompoundStatement,
        symbols: Map[AST.Identifier, IRSymbol]
    ): List[IR.Instruction] = {
      final case class DeclarationState(
          instructions: List[IR.Instruction],
          symbols: Map[AST.Identifier, IRSymbol]
      ) {
        def addInstructions(ni: List[IR.Instruction]): DeclarationState =
          DeclarationState(instructions ++ ni, symbols)
        def addSymbol(ident: AST.Identifier, symbol: IRSymbol): DeclarationState =
          DeclarationState(instructions, symbols + (ident -> symbol))
      }
      object DeclarationState {
        def apply(): DeclarationState = DeclarationState(Nil, Map())
      }

      // TODO: allocate all space for function at the same time (in all blocks)

      val declState = stmt.declarationList
        .map(_.declarations.toList)
        .getOrElse(Nil)
        .foldLeft(DeclarationState()) { case (acc, decl) =>
          decl.initDeclaratorList.map(_.declarators.toList).getOrElse(Nil).foldLeft(acc) {
            case (acc, initDecl) =>
              val ident = initDecl.declarator.identifier.get

              // TODO: fix alignment
              val localIndex = nextLocal()
              val localValue = IR.Value.Local(localIndex)
              val localTpe = translateType(initDecl.tpe.get)
              val alloc =
                IR.Op.Alloca(localTpe, Some(typeAlignment(localTpe))).instruction(localIndex)

              val exprGen = initDecl.initializer.map {
                case AST.Initializer.Expression(expr) =>
                  val gen = translateExpression(expr, acc.symbols)
                  val store = IR.Op
                    .Store(
                      false,
                      gen.tpe,
                      gen.value,
                      IR.Type.Pointer(gen.tpe),
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
        }

      val body = stmt.statementList
        .map(_.statements.toList)
        .getOrElse(Nil)
        .map(stmt => translateStatement(stmt, declState.symbols))
        .combineAll

      declState.instructions ++ body
    }

    def translateExpression(
        expr: AST.Expression,
        symbols: Map[AST.Identifier, IRSymbol]
    ): ExpressionGen = {
      val exprTpe = translateType(expr.tpe.get)
      expr match {
        case AST.Expression.Constant(c) =>
          c match {
            case AST.Constant.IntegerConstant(i) => ExpressionGen(Nil, IR.Value.Integer(i), exprTpe)
          }
        case AST.Expression.Identifier(ident) =>
          val entry = symbols.get(ident).get
          val index = nextLocal()
          val load = IR.Op
            .Load(
              false,
              entry.tpe,
              IR.Type.Pointer(entry.tpe),
              entry.value,
              Some(typeAlignment(entry.tpe))
            )
            .instruction(index)
          ExpressionGen(List(load), IR.Value.Local(index), entry.tpe)
        case AST.Expression.Plus(e1, e2) =>
          val gen1 = translateExpression(e1, symbols)
          val gen2 = translateExpression(e2, symbols)
          val resultLocal = nextLocal()
          val add = IR.Op.Add(exprTpe, gen1.value, gen2.value).instruction(resultLocal)
          ExpressionGen(gen1.code ++ gen2.code ++ List(add), IR.Value.Local(resultLocal), exprTpe)
        // TODO: is it possible to express this as a compositional fold?
        // Note how a variable reference loads from the memory location, whereas a pointer just returns the value
        case AST.Expression.Reference(rexpr) =>
          rexpr match {
            case AST.Expression.Identifier(ident) =>
              val symbol = symbols.get(ident).get
              ExpressionGen(Nil, symbol.value, IR.Type.Pointer(symbol.tpe))
          }
        // TODO: make this more compositional
        // TODO: this doesn't work on *&var
        case AST.Expression.Dereference(rexpr) =>
          val gen = translateExpression(rexpr, symbols)
          rexpr match {
            case AST.Expression.Identifier(ident) =>
              val symbol = symbols.get(ident).get
              // TODO: this needs to be consistent with symbol table
              val ptrTpe = translateType(rexpr.tpe.get)
              val derefTpe = translateType(expr.tpe.get)
              val ptrLocal = nextLocal()
              val derefLocal = nextLocal()
              val l1 = IR.Op
                .Load(
                  false,
                  ptrTpe,
                  IR.Type.Pointer(ptrTpe),
                  symbol.value,
                  Some(typeAlignment(ptrTpe))
                )
                .instruction(ptrLocal)
              val l2 = IR.Op
                .Load(
                  false,
                  derefTpe,
                  IR.Type.Pointer(derefTpe),
                  IR.Value.Local(ptrLocal),
                  Some(typeAlignment(derefTpe))
                )
                .instruction(derefLocal)
              ExpressionGen(List(l1, l2), IR.Value.Local(derefLocal), derefTpe)
          }
      }
    }

    val (ptes, rtpe) = fd.tpe.collect { case Type.Function(ptpes, rtpe) =>
      ptpes -> rtpe
    }.get
    val argNamesAndTypes = fd.declarator.functionParameters.get.map(_._1).zip(ptes)
    val irArguments = argNamesAndTypes.map(_._1).zip(ptes).map { (ident, tpe) =>
      IR.FunctionArgument(translateType(tpe), Nil, Some(ident.value))
    }
    val startingSymbols = argNamesAndTypes.map { (ident, tpe) =>
      val index = nextLocal()
      ident -> IRSymbol(ident, IR.Value.Local(index), translateType(tpe))
    }.toMap
    // arguments begin with %0, registers begin with %1
    if (local == -1) {
      local = 0
    }
    val instructions = translateCompoundStatement(fd.statements, startingSymbols)

    IR.FunctionDefinition(
      None,
      None,
      translateType(rtpe),
      fd.declarator.functionName.get.value,
      irArguments,
      instructions
    )
  }

  def translateType(tpe: Type): IR.Type =
    tpe match {
      case Type.Int          => IR.Type.Integer(32)
      case Type.Pointer(tpe) => IR.Type.Pointer(translateType(tpe))
      case _                 => ???
    }

  // TODO: depends on target/data layout? triples arm-none-eabi, x86_64-pc-linux-gnu
  def typeAlignment(tpe: IR.Type): Int =
    tpe match {
      case IR.Type.Integer(bits) => bits / 8
      case IR.Type.Pointer(_)    => 8
      case _                     => ???
    }

}
