package com.raasahsan.rcc

object Assembly {

  // TODO: program Monoid?
  final case class Program(lines: List[Line])

  enum Line {
    case Directive(value: Assembly.Directive)
    case Instruction(value: Assembly.Instruction)
    case Label(value: Assembly.Label)
  }

  enum Directive {
    case Global(name: String)
    case Text
    case Data
    case IntelSyntax
  }

  final case class Label(name: String) {
    def line: Line = Line.Label(this)
  }

  // TODO: instruction type and list of operands is more useful perhaps?
  enum Instruction {
    case Mov(dst: Operand.Destination, src: Operand.Source)
    case Add(dst: Operand.Destination, src: Operand.Source)
    case Sub(dst: Operand.Destination, src: Operand.Source)
    case Push(src: Operand.Source)
    case Pop(dst: Operand.Destination)
    case Syscall
    case Call(label: String)
    case Ret
    case Nop

    def line: Line = Line.Instruction(this)
  }

  final case class Register(name: String) {
    val operand = Operand.Register(this)
  }

  object Register {
    val r0 = Register("r0")
    val r1 = Register("r1")
    val r2 = Register("r2")
    val r3 = Register("r3")
    val r4 = Register("r4")
    val r5 = Register("r5")
    val r6 = Register("r6")
    val r7 = Register("r7")
    val r8 = Register("r8")
    val r9 = Register("r9")
    val r10 = Register("r10")
    val r11 = Register("r11")
    val r12 = Register("r12")
    val r13 = Register("r13")
    val r14 = Register("r14")
    val r15 = Register("r15")

    val rax = Register("rax")
    val rbx = Register("rbx")
    val rcx = Register("rcx")
    val rdx = Register("rdx")
    val rbp = Register("rbp")
    val rsp = Register("rsp")
    val rsi = Register("rsi")
    val rdi = Register("rdi")

    val eax = Register("eax")
    val ebx = Register("ebx")
    val ecx = Register("ecx")
    val edx = Register("edx")
    val ebp = Register("ebp")
    val esp = Register("esp")
    val esi = Register("esi")
    val edi = Register("edi")
  }

  sealed trait Operand

  object Operand {
    sealed trait Destination extends Operand
    sealed trait Source extends Operand

    final case class Immediate(imm: Assembly.Immediate) extends Source
    final case class Register(reg: Assembly.Register) extends Source with Destination
    final case class Address(addr: Assembly.Address) extends Source with Destination
  }

  final case class Immediate(value: Int) {
    def operand = Operand.Immediate(this)
  }

  enum Address {
    case Direct(addr: Int)
    case Indirect(reg: Register)
    case IndirectDisplacement(reg: Register, disp: Int)
    case IndirectDisplacementScaled(reg: Register, disp: Int, index: Register, scale: Int)

    def operand = Operand.Address(this)
  }

  def renderOperand(operand: Operand): String =
    operand match {
      case Operand.Immediate(imm) => s"${imm.value}"
      case Operand.Register(reg)  => s"${reg.name}"
      case Operand.Address(addr)  => s"DWORD PTR [${renderAddress(addr)}]"
    }

  def renderAddress(address: Address): String =
    address match {
      case Address.Direct(addr)  => s"$addr"
      case Address.Indirect(reg) => s"${reg.name}"
      case Address.IndirectDisplacement(reg, disp) =>
        val offset = if (disp > 0) s" + $disp" else s" - ${-disp}"
        s"${reg.name}$offset"
      case Address.IndirectDisplacementScaled(_, _, _, _) => ???
    }

  def renderDirective(dir: Directive): String =
    dir match {
      case Directive.Global(name) => s".global $name"
      case Directive.Text         => ".text"
      case Directive.Data         => ".data"
      case Directive.IntelSyntax  => ".intel_syntax noprefix"
    }

  def renderInstruction(instr: Instruction): String = {
    import Instruction._
    instr match {
      case Mov(dst, src) => s"mov ${renderOperand(dst)}, ${renderOperand(src)}"
      case Add(dst, src) => s"add ${renderOperand(dst)}, ${renderOperand(src)}"
      case Sub(dst, src) => s"sub ${renderOperand(dst)}, ${renderOperand(src)}"
      case Push(src)     => s"push ${renderOperand(src)}"
      case Pop(dst)      => s"pop ${renderOperand(dst)}"
      case Syscall       => "syscall"
      case Call(label)   => s"call $label"
      case Ret           => "ret"
      case Nop           => "nop"
    }
  }

  def renderProgram(program: Program): String =
    program.lines
      .map {
        case Line.Directive(dir)     => renderDirective(dir)
        case Line.Label(label)       => s"${label.name}:"
        case Line.Instruction(instr) => s"    ${renderInstruction(instr)}"
      }
      .mkString("", "\n", "\n")

  // TODO: typeclass for rendering?

}
