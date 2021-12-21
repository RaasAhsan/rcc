package com.raasahsan.rcc

import com.raasahsan.rcc.AST.ExternalDeclaration

object Main {

  val program = """
  
      int f(int a, int b) {
        return a + b;
      }
      int main() {
        return 3;
      }
  
  """

  def main(args: Array[String]): Unit = {
    val result = Parser.parse(program)
    // println(result)

    val p = result.toOption.get
    val fds = p.externalDeclarations.toList.collect {
      case ExternalDeclaration.FunctionDefinition(fd) => fd
    }

    val gen = fds.map(fd => Assembly.renderProgram(Generator.generateFunctionDefinition(fd)))

    println(gen)

    // println(result.left.map(_.expected.map(_.context)))
  }
}
