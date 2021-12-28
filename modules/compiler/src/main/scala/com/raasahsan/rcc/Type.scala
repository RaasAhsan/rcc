package com.raasahsan.rcc

// TODO: struct, union, typedef / user-defined types?
enum Type {
  case Char
  case Short
  case Int
  case Long
  case Float
  case Double
  case Void
  case Array(base: Type)
  case Pointer(base: Type)
  case Function(params: List[Type], returnType: Type)
}
