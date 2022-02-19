package com.raasahsan.rcc

// TODO: struct, union, typedef / user-defined types?
enum Type {
  case Char
  case Short
  case Int
  case UnsignedInt
  case Long
  case Float
  case Double
  case Void
  case Array(base: Type)
  case Pointer(base: Type)
  case Function(params: List[Type], returnType: Type)

  def size: Int =
    this match {
      case Char           => 1
      case Short          => 2
      case Int            => 4
      case UnsignedInt    => 4
      case Long           => 8
      case Float          => 4
      case Double         => 8
      case Void           => ???
      case Array(_)       => 8
      case Pointer(_)     => 8
      case Function(_, _) => 8
    }
}
