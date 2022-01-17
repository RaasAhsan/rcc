package com.raasahsan.rcc

import com.raasahsan.rcc.AST.ExternalDeclaration
import com.raasahsan.rcc.codegen.llvm._

object Main {

  val program = """
  
    int main() {
      int x = 3;
      return x;
    }
  """

  val program2 = """
      int puts(char *s);
      int putchar(char s);

      unsigned int* baseaddr = (unsigned int*) 0xb9000;

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
        char* str = "hellosworld";
        char c = *str;
        puts(str);
        putchar(c);
        return 3;
      }
  
  """

  def main(args: Array[String]): Unit = {
    // println(program.substring(278))
    val result = Parser.parse(program)
    println(result)

    val p = result.toOption.get
    val typed = Typer.typeCheck(p)
    println(typed)

    // val generator = new Generator
    // val gen = generator.generateTranslationUnit(p)
    // val render = Assembly.renderProgram(gen)

    val module = LLVMBackend.translate(p)

    println(module)
    val render = LLVMRenderer.render(module)

    val outDir = os.pwd / "examples"
    os.write.over(outDir / "simple.ll", render)

    println("Wrote file")
  }
}
