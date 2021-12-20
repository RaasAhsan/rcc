package com.raasahsan.rcc

object NASM {

  final case class Program()

  enum Directive {
    case Global(name: String)
  }

  // TODO: ADT over instruction?
  final case class Line(label: Option[String], instr: Instruction, operands: List[String])

  final case class Instruction()

}
