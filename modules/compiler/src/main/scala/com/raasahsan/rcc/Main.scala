package com.raasahsan.rcc

object Main {

  val program = """
  
  int main() {
    return 3;
  }
  
  """
  
  def main(args: Array[String]): Unit = {
    println(Parser.parse(program))
    println(Parser.parse(program).left.map(_.expected.map(_.context)))
  }
}
