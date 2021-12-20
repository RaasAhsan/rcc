package com.raasahsan.rcc

import munit.FunSuite

class ParserSuite extends FunSuite {

  test("bare") {
    val program = """
      int main() {
        return 3;
      }
    """
    assert(Parser.parse(program).isRight)
  }

  test("assignment") {
    val program = """
      int main() {
        int b = 4;
        return b;
      }
    """
    assert(Parser.parse(program).isRight)
  }

  test("arithmetic") {
    val program = """
      int main() {
        return 4 + 16 * 3 - 3 / 2;
      }
    """
    assert(Parser.parse(program).isRight)
  }

  test("grouping") {
    val program = """
      int main() {
        return (4 + 16) * (3 - 3) / 2;
      }
    """
    assert(Parser.parse(program).isRight)
  }

}
