package com.raasahsan.rcc

enum RegisterAssignment {
  case Memory(addr: Assembly.SizedAddress)
  case Register(reg: Assembly.Register)
  case Label(label: Assembly.Label)
  case Constant(value: Int)
}
