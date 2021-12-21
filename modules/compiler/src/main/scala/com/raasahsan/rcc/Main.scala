package com.raasahsan.rcc

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
    println(program.substring(42))
    println(result)
    // println(result.left.map(_.expected.map(_.context)))
  }
}
