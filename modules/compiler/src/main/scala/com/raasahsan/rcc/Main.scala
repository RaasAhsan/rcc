package com.raasahsan.rcc

object Main {

  val program = """
  
      int main() {
        int a = 35;int b = 35;
        return b;
      }
  
  """

  def main(args: Array[String]): Unit = {
    val result = Parser.parse(program)
    println(program.substring(42))
    println(result)
    // println(result.left.map(_.expected.map(_.context)))
  }
}
