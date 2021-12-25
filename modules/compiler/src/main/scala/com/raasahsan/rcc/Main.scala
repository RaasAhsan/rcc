package com.raasahsan.rcc

import com.raasahsan.rcc.AST.ExternalDeclaration

object Main {

  val program = """

      int add(int x, int y) {
        return x + y;
      }
  
      int main() {
        int a = 5;
        int b = 6;
        int c = add(a, b);
        return c;
      }
  
  """

  def main(args: Array[String]): Unit = {
    val result = Parser.parse(program)
    println(result)

    val p = result.toOption.get
    val gen = Generator.generateTranslationUnit(p)
    val render = Assembly.renderProgram(Assembly.Program(gen))

    println(render)

    // println(result.left.map(_.expected.map(_.context)))
  }
}
