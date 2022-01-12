package com.raasahsan.llvm

object IR {

  final case class Module()
  
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

    // Single value types
    case Integer(bits: Int)

    // Aggregate types
    case Array(num: Int, ty: Type)

    case LiteralStructure(tpes: List[Type], packed: Boolean)
    case IdentifiedStructure(name: String)
    case OpaqueStructure
  }

  enum Instruction {
    // Terminator instructions
    case Ret(ret: IR.Return)
    case Br(br: IR.Branch)

    // Binary operations
    case Add(tpe: Type, op1: String, op2: String)

    // Memory operations
    case Alloca(tpe: Type, alignment: Option[Int])
    case Load(volatile: Boolean, tpe: Type, ptrTpe: Type, ptr: String)
    case Store(volatile: Boolean, tpe: Type, value: String, ptrTpe: Type, ptr: String)
    case Getelementptr()

    // Conversion operations
    case Inttoptr(tpe: Type, value: String, ptrTpe: Type)
  }

  enum Return {
    case Void
    case Value(tpe: Type, value: String)
  }

  enum Branch {
    case Unconditional(label: String)
    case Conditional(cond: String, trueLabel: String, falseLabel: String)
  }

}
