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

  test("parse assignment") {
    val program = """
      int main() {
        int b = 4;
        return b;
      }
    """
  }

}
