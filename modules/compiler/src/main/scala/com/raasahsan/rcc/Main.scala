package com.raasahsan.rcc

object Main {

  val program = """
  
  int main() {
    return 3;
  }
  
  """
  
  def main(args: Array[String]): Unit = {
    val result = Parser.parse(program)
    println(result)
    println(result.left.map(_.expected.map(_.context)))
  }
}
