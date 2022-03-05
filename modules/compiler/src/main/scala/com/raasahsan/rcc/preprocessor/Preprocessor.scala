package com.raasahsan.rcc.preprocessor

import cats.syntax.all._

object Preprocessor {

  import IR._

  def trailingWhitespace(input: String): String =
    if (input.endsWith("\n")) input else input + "\n"

  def parse(input: String): Either[String, PreprocessingFile] = {
    val file = Parser.parse(trailingWhitespace(input))
    println(file)
    file.leftMap(_ => "failed to preprocess file")
  }

  def preprocess(input: String, includePath: os.Path, workingPath: os.Path): Either[String, String] =
    parse(input).flatMap { file =>
      file.groups.fold[Either[String, String]](Right(""))(g => interpret(g, includePath, workingPath))
    }

  def interpret(group: Group, includePath: os.Path, workingPath: os.Path): Either[String, String] =
    group.parts.toList.traverse {
      case GroupPart.Text(text) => Right(text.getOrElse(""))
      case GroupPart.Control(line) =>
        line match {
          case ControlLine.Include(header) =>
            header match {
              case HeaderName.Q(name) => 
                // TODO: process errors
                val filePath = workingPath / name
                val contents = os.read(filePath)
                preprocess(contents, includePath, workingPath)
              case _ => throw new RuntimeException("unimplemented header")
            }
        }
      case _ => throw new RuntimeException("unimplemented part")
    }.map(_.mkString("\n"))

}
