package com.raasahsan.rcc

import com.raasahsan.rcc.AST.ExternalDeclaration

object Main {

  val program = """

      int add(int x, int y) {
        return x + y;
      }

      int foo(int x) {
        if (x) {
          return 3;
        } else {
          return 4;
        }
      }
  
      int main() {
        int z = foo(1);
        return z;
      }
  
  """

  def main(args: Array[String]): Unit = {
    val result = Parser.parse(program)
    println(result)

    val p = result.toOption.get
    val gen = Generator.generateTranslationUnit(p)
    val render = Assembly.renderProgram(gen)

    val outDir = os.pwd / "examples"
    os.write.over(outDir / "simple.asm", render)

    println("Wrote file")
  }
}
