package com.raasahsan.rcc.preprocessor

import munit.FunSuite
import cats.syntax.all._
import cats.data.NonEmptyList

class PreprocessorSuite extends FunSuite {

  import IR._

  test("bare program") {
    val program = "int main(){return 0;}"
    assertEquals(Preprocessor.preprocess(program).void, Right(()))
  }

  test("leading whitespace") {
    val program = "\n  int main(){return 0;}"
    assertEquals(Preprocessor.preprocess(program).void, Right(()))
  }

  test("trailing whitespace") {
    val program = "int main(){return 0;} "
    assertEquals(Preprocessor.preprocess(program).void, Right(()))
  }

  test("include") {
    val program = "#include <stdio.h>\nint main(){return 0;}"
    assertEquals(
      Preprocessor.parse(program),
      Right(
        PreprocessingFile(
          Some(
            Group(
              NonEmptyList.of(
                GroupPart.Control(ControlLine.Include(HeaderName.H("stdio.h"))),
                GroupPart.Text(Some("int main(){return 0;}"))
              )
            )
          )
        )
      )
    )
  }

}
