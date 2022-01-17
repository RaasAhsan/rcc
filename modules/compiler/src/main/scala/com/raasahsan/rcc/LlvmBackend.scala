package com.raasahsan.rcc

import cats.syntax.all._

import com.raasahsan.llvm.IR

object LLVMBackend {

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
              val alloc = IR.Op.Alloca(localTpe, Some(4)).instruction(localIndex)

              val exprGen = initDecl.initializer.map {
                case AST.Initializer.Expression(expr) =>
                  val gen = translateExpression(expr, acc.symbols)
                  val store = IR.Op
                    .Store(false, gen.tpe, gen.value, IR.Type.Pointer(gen.tpe), localValue)
                    .instruction
                  gen.code ++ List(store)
                case _ => ???
              }
              val symbol = IRSymbol(ident, localValue, localTpe)
              acc.addInstructions(exprGen.getOrElse(Nil)).addSymbol(ident, symbol)
          }
        }

      stmt.statementList
        .map(_.statements.toList)
        .getOrElse(Nil)
        .map(stmt => translateStatement(stmt, declState.symbols))
        .combineAll
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
          ExpressionGen(Nil, entry.value, entry.tpe)
        case AST.Expression.Plus(e1, e2) =>
          val gen1 = translateExpression(e1, symbols)
          val gen2 = translateExpression(e2, symbols)
          val resultLocal = nextLocal()
          val add = IR.Op.Add(exprTpe, gen1.value, gen2.value).instruction(resultLocal)
          ExpressionGen(gen1.code ++ gen2.code ++ List(add), IR.Value.Local(resultLocal), exprTpe)
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
      case Type.Int => IR.Type.Integer(32)
      case _        => ???
    }

}
