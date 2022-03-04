package com.raasahsan.rcc.preprocessor

object Preprocessor {
  
  def preprocess(input: String): String = {
    val paddedInput = if (input.endsWith("\n")) input else input + "\n"
    val file = Parser.parse(paddedInput)
    println(file)
    ""
  }

}
