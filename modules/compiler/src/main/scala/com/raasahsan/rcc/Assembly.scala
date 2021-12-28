package com.raasahsan.rcc

import cats.Monoid
import cats.syntax.all._

object Assembly {

  final case class Lines(lines: List[Line])

  object Lines {
    val empty = Lines(Nil)
  }

  trait ToLines[A] {
    def lines(a: A): Lines
  }

  implicit val catsMonoidForLines: Monoid[Lines] = new Monoid[Lines] {
    override def empty: Lines = Lines.empty
    override def combine(a: Lines, b: Lines): Lines = Lines(a.lines ++ b.lines)
  }

  implicit val toLinesForLine: ToLines[Line] = new ToLines[Line] {
    override def lines(a: Line): Lines = Lines(List(a))
  }

  implicit val toLinesForLines: ToLines[Lines] = new ToLines[Lines] {
    override def lines(a: Lines): Lines = a
  }

  implicit val toLinesForInstruction: ToLines[Instruction] = new ToLines[Instruction] {
    override def lines(a: Instruction): Lines = Lines(List(a.line))
  }

  implicit val toLinesForDirective: ToLines[Directive] = new ToLines[Directive] {
    override def lines(a: Directive): Lines = Lines(List(a.line))
  }

  implicit val toLinesForLabel: ToLines[Label] = new ToLines[Label] {
    override def lines(a: Label): Lines = Lines(List(a.line))
  }

  // TODO: generate boilerplate
  def instructions[A0: ToLines](a0: A0): Lines =
    implicitly[ToLines[A0]].lines(a0)

  def instructions[A0: ToLines, A1: ToLines](a0: A0, a1: A1): Lines =
    Monoid.combineAll(
      List(
        implicitly[ToLines[A0]].lines(a0),
        implicitly[ToLines[A1]].lines(a1)
      )
    )

  def instructions[A0: ToLines, A1: ToLines, A2: ToLines](a0: A0, a1: A1, a2: A2): Lines =
    Monoid.combineAll(
      List(
        implicitly[ToLines[A0]].lines(a0),
        implicitly[ToLines[A1]].lines(a1),
        implicitly[ToLines[A2]].lines(a2)
      )
    )

  def instructions[A0: ToLines, A1: ToLines, A2: ToLines, A3: ToLines](
      a0: A0,
      a1: A1,
      a2: A2,
      a3: A3
  ): Lines =
    Monoid.combineAll(
      List(
        implicitly[ToLines[A0]].lines(a0),
        implicitly[ToLines[A1]].lines(a1),
        implicitly[ToLines[A2]].lines(a2),
        implicitly[ToLines[A3]].lines(a3)
      )
    )

  def instructions[A0: ToLines, A1: ToLines, A2: ToLines, A3: ToLines, A4: ToLines](
      a0: A0,
      a1: A1,
      a2: A2,
      a3: A3,
      a4: A4
  ): Lines =
    Monoid.combineAll(
      List(
        implicitly[ToLines[A0]].lines(a0),
        implicitly[ToLines[A1]].lines(a1),
        implicitly[ToLines[A2]].lines(a2),
        implicitly[ToLines[A3]].lines(a3),
        implicitly[ToLines[A4]].lines(a4)
      )
    )

  def instructions[A0: ToLines, A1: ToLines, A2: ToLines, A3: ToLines, A4: ToLines, A5: ToLines](
      a0: A0,
      a1: A1,
      a2: A2,
      a3: A3,
      a4: A4,
      a5: A5
  ): Lines =
    Monoid.combineAll(
      List(
        implicitly[ToLines[A0]].lines(a0),
        implicitly[ToLines[A1]].lines(a1),
        implicitly[ToLines[A2]].lines(a2),
        implicitly[ToLines[A3]].lines(a3),
        implicitly[ToLines[A4]].lines(a4),
        implicitly[ToLines[A5]].lines(a5)
      )
    )

  def instructions[
      A0: ToLines,
      A1: ToLines,
      A2: ToLines,
      A3: ToLines,
      A4: ToLines,
      A5: ToLines,
      A6: ToLines,
      A7: ToLines
  ](
      a0: A0,
      a1: A1,
      a2: A2,
      a3: A3,
      a4: A4,
      a5: A5,
      a6: A6,
      a7: A7
  ): Lines =
    Monoid.combineAll(
      List(
        implicitly[ToLines[A0]].lines(a0),
        implicitly[ToLines[A1]].lines(a1),
        implicitly[ToLines[A2]].lines(a2),
        implicitly[ToLines[A3]].lines(a3),
        implicitly[ToLines[A4]].lines(a4),
        implicitly[ToLines[A5]].lines(a5),
        implicitly[ToLines[A6]].lines(a6),
        implicitly[ToLines[A7]].lines(a7)
      )
    )

  def instructions[
      A0: ToLines,
      A1: ToLines,
      A2: ToLines,
      A3: ToLines,
      A4: ToLines,
      A5: ToLines,
      A6: ToLines,
      A7: ToLines,
      A8: ToLines
  ](
      a0: A0,
      a1: A1,
      a2: A2,
      a3: A3,
      a4: A4,
      a5: A5,
      a6: A6,
      a7: A7,
      a8: A8
  ): Lines =
    Monoid.combineAll(
      List(
        implicitly[ToLines[A0]].lines(a0),
        implicitly[ToLines[A1]].lines(a1),
        implicitly[ToLines[A2]].lines(a2),
        implicitly[ToLines[A3]].lines(a3),
        implicitly[ToLines[A4]].lines(a4),
        implicitly[ToLines[A5]].lines(a5),
        implicitly[ToLines[A6]].lines(a6),
        implicitly[ToLines[A7]].lines(a7),
        implicitly[ToLines[A8]].lines(a8)
      )
    )

  // Comments, commented lines, block comments, etc.
  enum Line {
    case Directive(value: Assembly.Directive)
    case Instruction(value: Assembly.Instruction)
    case Label(value: Assembly.Label)
    case Empty
  }

  enum Directive {
    case Global(name: String)
    case Text
    case Data
    case IntelSyntax
    case StringLiteral(value: String)

    def line: Line = Line.Directive(this)
  }

  final case class Label(name: String) {
    def line: Line = Line.Label(this)
    def operand = Operand.Label(this)
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
    case Cmp(first: Operand, second: Operand)
    case Jmp(label: String)
    case Je(label: String)
    case Jne(label: String)

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

    val e8 = Register("e8")
    val e9 = Register("e9")
  }

  sealed trait Operand

  // addressing modes
  // expression?
  object Operand {
    sealed trait Destination extends Operand
    sealed trait Source extends Operand

    final case class Immediate(imm: Assembly.Immediate) extends Source
    final case class Register(reg: Assembly.Register) extends Source with Destination
    final case class Address(addr: Assembly.Address) extends Source with Destination
    final case class Label(label: Assembly.Label) extends Source
  }

  final case class Immediate(value: Int) {
    def operand = Operand.Immediate(this)
  }

  // effective addresses
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
      case Operand.Label(label) => label.name
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
      case Directive.StringLiteral(value) => s".string \"$value\""
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
      case Cmp(a, b)     => s"cmp ${renderOperand(a)}, ${renderOperand(b)}"
      case Jmp(label)    => s"jmp $label"
      case Je(label)     => s"je $label"
      case Jne(label)    => s"jne $label"
    }
  }

  def renderProgram(lines: Lines): String =
    lines.lines
      .map {
        case Line.Directive(dir)     => renderDirective(dir)
        case Line.Label(label)       => s"${label.name}:"
        case Line.Instruction(instr) => s"    ${renderInstruction(instr)}"
        case Line.Empty              => ""
      }
      .mkString("", "\n", "\n")

  // TODO: typeclass for rendering?

}
