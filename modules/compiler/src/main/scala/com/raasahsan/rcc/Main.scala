package com.raasahsan.rcc

object Main {

  val program = """
  
      int main() {
        int a = 3 * 5;
        return b;
      }
  
  """

  def main(args: Array[String]): Unit = {
    val result = Parser.parse(program)
    println(result)
    println(result.left.map(_.expected.map(_.context)))
  }
}
