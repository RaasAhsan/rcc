package com.raasahsan.rcc

import scala.collection.mutable

import Assembly._
import StackFrame._

class StackFrame {

  val symbols = new mutable.HashMap[String, Allocation]
  var stackOffset = 0

  // TODO: rsp is 64-bit but we're using 32-bit values for now
  def alignedStackOffset: Int =
    ((stackOffset + 8) & 0xfffffff0) + 8

  def allocateNamed(name: String, size: DataSize): Allocation = {
    val assign = allocate(size)
    symbols.put(name, assign)
    assign
  }

  def allocate(size: DataSize): Allocation = {
    stackOffset += size.size
    Allocation(RegisterAssignment.Memory(Address.IndirectDisplacement(Register.rbp, -stackOffset)), size)
  }

  def get(name: String): Allocation =
    symbols.get(name).get

}

object StackFrame {
  case class Allocation(assignment: RegisterAssignment, size: DataSize)
}
