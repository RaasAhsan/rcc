package com.raasahsan.llvm

object IR {
  
  enum Linkage {
    case External
  }

  enum CallingConvention {
    case C
  }

  final case class FunctionDefinition(linkage: Option[Linkage], cconv: Option[CallingConvention])

  final case class FunctionArgument(tpe: Type, attrs: List[Unit], name: Option[String])

  enum Type {
    case Void
    case Function(returnType: Type, parameters: List[Type])
    case Integer(bits: Int)
  }

}
