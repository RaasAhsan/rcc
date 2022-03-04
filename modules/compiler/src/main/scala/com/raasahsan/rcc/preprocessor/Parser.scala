package com.raasahsan.rcc.preprocessor

import cats.data.NonEmptyList
import cats.syntax.all._
import cats.parse.{Parser => P, Parser0 => P0}

object Parser {

  import IR._

  def parse(input: String) =
    preprocessingFile.parseAll(input)

  def preprocessingFile: P0[PreprocessingFile] =
    group.?.map(PreprocessingFile(_))

  def group: P[Group] =
    groupPart.rep.map(Group(_))

  def groupPart: P[GroupPart] =
    controlLine.map(GroupPart.Control(_)).backtrack |
      (text.?.with1 <* newline).map(GroupPart.Text(_))

  def controlLine: P[ControlLine] = {
    pound.surroundedBy(horizontalWhitespace.?) *>
      (include *> horizontalWhitespace.? *> headerName).map(ControlLine.Include(_))
      <* (horizontalWhitespace.? ~ newline)
  }

  def ifSection: P[IfSection] = ???

  def text: P[String] =
    P.charsWhile(_ != '\n')

  def headerName: P[HeaderName] = {
    def hCharSequence: P[String] =
      P.charsWhile(c => c != '>' && c != '\n')

    def qCharSequence: P[String] =
      P.charsWhile(c => c != '\"' && c != '\n')

    (leftAngle *> hCharSequence <* rightAngle).map(HeaderName.H(_)) |
      (doubleQuote *> qCharSequence <* doubleQuote).map(HeaderName.Q(_))
  }

  // Keywords

  def pound: P[Unit] =
    P.char('#')

  def leftAngle: P[Unit] =
    P.char('<')

  def rightAngle: P[Unit] =
    P.char('>')

  def doubleQuote: P[Unit] =
    P.char('\"')

  def include: P[Unit] =
    P.string("include")

  def newline: P[Unit] = P.char('\n')

  def horizontalWhitespace: P[Unit] =
    P.charsWhile(c => c == ' ' || c == '\t').void

  def whitespace: P[Unit] =
    P.charsWhile(_.isWhitespace).void

  def maybeWhitespace: P0[Unit] =
    whitespace.?.void

}
