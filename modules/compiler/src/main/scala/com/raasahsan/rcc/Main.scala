package com.raasahsan.rcc

import com.raasahsan.rcc.AST.ExternalDeclaration
import com.raasahsan.rcc.codegen.llvm._

object Main {

  val program = """
  
    int main() {
      int x = 3;
      int* h = &x;
      return *h;
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

    val ir = IRTranslation.translateTranslationUnit(p)
    println(ir)

    val typed = Typer.typeCheck(ir)
    println(typed)
    
    val module = LLIRTranslation.translate(ir)

    println(module)
    val render = LLVMRenderer.render(module)

    val outDir = os.pwd / "examples"
    os.write.over(outDir / "simple.ll", render)

    println("Wrote file")
  }
}
