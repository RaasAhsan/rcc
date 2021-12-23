package com.raasahsan.rcc

import com.raasahsan.rcc.AST.ExternalDeclaration

object Main {

  val program = """
  
      int main() {
        int a = 5;
        int b = 6;
        int c = a + b;
        int d = c + b;
        int e = d + c;
        return d;
      }
  
  """

  def main(args: Array[String]): Unit = {
    val result = Parser.parse(program)
    println(result)

    val p = result.toOption.get
    val fds = p.externalDeclarations.toList.collect {
      case ExternalDeclaration.FunctionDefinition(fd) => fd
    }

    val gen = fds.map(fd => Assembly.renderProgram(Generator.generateFunctionDefinition(fd)))

    println(gen)

    // println(result.left.map(_.expected.map(_.context)))
  }
}
