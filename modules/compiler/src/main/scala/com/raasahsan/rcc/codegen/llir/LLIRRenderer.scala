package com.raasahsan.rcc.codegen.llir

object LLIRRenderer {

  import LLIR._

  def render(module: Module): String =
    module.topLevelDeclarations
      .map { case TopLevelDeclaration.FunctionDefinition(fd) =>
        val args =
          fd.arguments.map(arg => s"${renderType(arg.tpe)} %${arg.name.get}").mkString(", ")
        val start = s"define ${renderType(fd.resultType)} @${fd.name}($args) {\n"
        val end = "}\n"

        val instrs = fd.instructions.map(i => "  " + renderInstruction(i) + "\n")
        instrs.mkString(start, "", end)
      }
      .mkString("\n")

  def renderInstruction(instr: Instruction): String =
    instr.index.map(i => s"%$i = ").getOrElse("") + renderOp(instr.op)

  def renderOp(op: Op): String =
    op match {
      case Op.Ret(ret) =>
        ret match {
          case Return.Void          => "ret"
          case Return.Value(tpe, v) => s"ret ${renderType(tpe)} ${renderValue(v)}"
        }
      case Op.Add(tpe, v1, v2) => s"add ${renderType(tpe)} ${renderValue(v1)}, ${renderValue(v2)}"
      case Op.Alloca(tpe, align) =>
        val renderAlign = align.fold("")(a => s", align $a")
        s"alloca ${renderType(tpe)}$renderAlign"
      case Op.Store(vol, tpe, value, ptrTpe, ptr, align) =>
        val renderAlign = align.fold("")(a => s", align $a")
        s"store ${renderType(tpe)} ${renderValue(value)}, ${renderType(ptrTpe)} ${renderValue(ptr)}$renderAlign"
      case Op.Load(vol, tpe, ptrTpe, ptr, align) =>
        val renderAlign = align.fold("")(a => s", align $a")
        s"load ${renderType(tpe)}, ${renderType(ptrTpe)} ${renderValue(ptr)}$renderAlign"
      case Op.Inttoptr(srcTpe, value, targetTpe) =>
        s"inttoptr ${renderType(srcTpe)} ${renderValue(value)} to ${renderType(targetTpe)}"
      case _ => ???
    }

  def renderValue(v: Value): String =
    v match {
      case Value.Integer(i) => s"$i"
      case Value.Local(i)   => s"%$i"
    }

  def renderType(tpe: Type): String =
    tpe match {
      case Type.Integer(bits) => s"i$bits"
      case Type.Pointer(utpe) => s"${renderType(utpe)}*"
      case Type.Void          => "void"
      case _                  => ???
    }

}
