package com.raasahsan.rcc.preprocessor

import cats.data.NonEmptyList

object IR {
  
  final case class PreprocessingFile(groups: Option[Group])

  final case class Group(parts: NonEmptyList[GroupPart])

  enum GroupPart { 
    case Text(value: Option[String])
    case If(section: IfSection)
    case Control(line: ControlLine)
  }

  final case class IfSection(ifGroup: IfGroup, elifGroups: Option[NonEmptyList[ElifGroup]], elseGroup: Option[ElseGroup])

  enum IfGroup {
    case IfExpr(expr: ConstantExpression, group: Option[Group])
    case Ifdef(ident: Identifier, group: Option[Group])
    case Ifndef(ident: Identifier, group: Option[Group])
  }

  final case class Identifier(value: String)

  enum ConstantExpression {
    case Add()
  }

  final case class ElifGroup(expr: ConstantExpression, group: Option[Group])

  final case class ElseGroup(group: Option[Group])

  // A token can be whitespace. Whitespace is only preserved for preprocessing tokens
  // that are considered controlled text (i.e. not for any directives).
  final case class Token(value: String)

  enum ControlLine {
    case Include(header: HeaderName)
  }

  enum HeaderName {
    case H(value: String)
    case Q(value: String)
  }

}
