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
        return (4 + 16) * (3 + 3);
      }
    """
    assert(Parser.parse(program).isRight)
  }

  test("compound") {
    val program = """
      int main() {
        int a = 3;
        int b = 5;
        return a + b;
      }
    """
    assert(Parser.parse(program).isRight)
  }

  test("multiple functions") {
    val program = """
      int f(int a, int b) {
        return a + b;
      }
      int main() {
        return 3;
      }
    """
    assert(Parser.parse(program).isRight)
  }

  test("function call") {
    val program = """
      int main() {
        return f(2, 3);
      }
    """
    assert(Parser.parse(program).isRight)
  }

  test("array access") {
    val program = """
      int main() {
        return array[a + b];
      }
    """
    assert(Parser.parse(program).isRight)
  }

  test("casting") {
    val program = """
      int main() {
        int* a = (int*) 0x9000;
        return 3;
      }
    """
    assert(Parser.parse(program).isRight)
  }
}
