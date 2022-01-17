package com.raasahsan.rcc

import com.raasahsan.llvm.IR

object LLVMBackend {

  def translate(translationUnit: AST.TranslationUnit): IR.Module = ???

  def translateFunctionDefinition(fd: AST.FunctionDefinition): IR.FunctionDefinition = {
    var local: Int = -1

    def nextLocal(): Int = {
      local += 1
      local
    }

    def translateStatement(stmt: AST.Statement): List[IR.Instruction] = {
      stmt match {
        case AST.Statement.Expression(expr) =>
          expr.expr.map(e => translateExpression(e)).getOrElse(Nil)
        case AST.Statement.Compound(compound) =>
          
          ???
      }
    }

    def translateExpression(expr: AST.Expression): List[IR.Instruction] = {
      final case class Gen(code: List[IR.Instruction], value: IR.Value, tpe: IR.Type)

      def go(e: AST.Expression): Gen = {
        val exprTpe = translateType(expr.tpe.get)
        e match {
          case AST.Expression.Constant(c) =>
            c match {
              case AST.Constant.IntegerConstant(i) => Gen(Nil, IR.Value.Integer(i), exprTpe)
            }
          case AST.Expression.Plus(e1, e2) => 
            val gen1 = go(e1)
            val gen2 = go(e2)
            val resultLocal = nextLocal()
            val add = IR.Op.Add(exprTpe, gen1.value, gen2.value).instruction(resultLocal)
            Gen(gen1.code ++ gen2.code ++ List(add), IR.Value.Local(resultLocal), exprTpe)
        }
      }

      go(expr).code
    }

    ???
  }

  def translateType(tpe: Type): IR.Type = 
    tpe match {
      case Type.Int => IR.Type.Integer(32)
      case _ => ???
    }

}
