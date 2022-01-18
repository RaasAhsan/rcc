package com.raasahsan.rcc.codegen.llvm

object IR {

  final case class Module(topLevelDeclarations: List[TopLevelDeclaration])

  enum TopLevelDeclaration {
    case FunctionDefinition(fd: IR.FunctionDefinition)
  }

  enum Linkage {
    case External
  }

  enum CallingConvention {
    case C
  }

  final case class FunctionDefinition(
      linkage: Option[Linkage],
      cconv: Option[CallingConvention],
      resultType: Type,
      name: String,
      arguments: List[FunctionArgument],
      instructions: List[Instruction]
  )

  final case class FunctionArgument(tpe: Type, attrs: List[Unit], name: Option[String])

  enum Type {
    case Void
    case Function(returnType: Type, parameters: List[Type])

    // Single value types
    case Integer(bits: Int)

    case Pointer(tpe: Type)

    // Aggregate types
    case Array(num: Int, ty: Type)

    case LiteralStructure(tpes: List[Type], packed: Boolean)
    case IdentifiedStructure(name: String)
    case OpaqueStructure
  }

  final case class Instruction(index: Option[Int], op: Op)

  enum Op {
    // Terminator instructions
    case Ret(ret: IR.Return)
    case Br(br: IR.Branch)

    // Binary operations
    case Add(tpe: Type, op1: Value, op2: Value)

    // Memory operations
    case Alloca(tpe: Type, alignment: Option[Int])
    case Load(volatile: Boolean, tpe: Type, ptrTpe: Type, ptr: Value, alignment: Option[Int])
    case Store(volatile: Boolean, tpe: Type, value: Value, ptrTpe: Type, ptr: Value, alignment: Option[Int])
    case Getelementptr()

    // Conversion operations
    case Inttoptr(tpe: Type, value: String, ptrTpe: Type)

    def instruction: Instruction = Instruction(None, this)
    def instruction(index: Int): Instruction = Instruction(Some(index), this)
  }

  enum Return {
    case Void
    case Value(tpe: Type, value: IR.Value)
  }

  enum Branch {
    case Unconditional(label: String)
    case Conditional(cond: String, trueLabel: String, falseLabel: String)
  }

  enum Value {
    case Local(index: Int)
    case Integer(value: Int)
  }

}
