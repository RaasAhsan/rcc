package com.raasahsan.rcc

import munit.FunSuite

class ParserSuite extends FunSuite {
  
  test("parse simple program") {
    val program = """
      int main() {
        return 3;
      }
    """
    assert(Parser.parse(program).isRight)
  }

}
