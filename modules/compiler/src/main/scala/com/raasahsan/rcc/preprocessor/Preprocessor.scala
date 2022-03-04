package com.raasahsan.rcc.preprocessor

import cats.syntax.all._

object Preprocessor {

  import IR._

  def parse(input: String): Either[String, PreprocessingFile] = {
    val paddedInput = if (input.endsWith("\n")) input else input + "\n"
    val file = Parser.parse(paddedInput)
    println(file)
    file.leftMap(_ => "failed to preprocess file")
  }

  def preprocess(input: String): Either[String, String] = {
    parse(input).map(_ => "")
  }

}
