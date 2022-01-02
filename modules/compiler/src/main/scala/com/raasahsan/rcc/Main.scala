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
        } else if (x + 1) {
          return 4;
        } else {
          return 5;
        }
      }
  
      int main() {
        char* str = "helloworld";
        return 3;
      }
  
  """

  def main(args: Array[String]): Unit = {
    println(program.substring(278))
    val result = Parser.parse(program)
    // println(result)

    val p = result.toOption.get
    val typed = Typer.typeCheck(p)
    println(typed)

    val generator = new Generator
    val gen = generator.generateTranslationUnit(p)
    val render = Assembly.renderProgram(gen)

    val outDir = os.pwd / "examples"
    os.write.over(outDir / "simple.asm", render)

    println("Wrote file")
  }
}
