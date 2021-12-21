package com.raasahsan.rcc

object Assembly {

  final case class Program()

  enum Directive {
    case Global(name: String)
  }

  // TODO: ADT over instruction?
  final case class Line(label: Option[String], instr: Instruction, operands: List[String])

  enum Instruction {
    case Mov(a: Operand, b: Operand)
    case Sub(a: Operand, b: Operand)
  }

  enum Register {
    case Rax
    case Rbx
    case Rcx
    case Rdx
    case Rbp
    case Rsp
    case Rsi
  }

  enum Operand {
    case Immediate(value: Int)
    case Register(register: Assembly.Register)
    case Memory(address: Int)
  }

  enum Immediate {
    case Decimal(value: Int)
  }

}
