package com.raasahsan.rcc

import scala.collection.mutable

class StackFrame {

  import Assembly._

  val symbols = new mutable.HashMap[String, RegisterAssignment]
  var stackOffset = 0

  // TODO: rsp is 64-bit but we're using 32-bit values for now
  def alignedStackOffset: Int =
    ((stackOffset + 8) & 0xfffffff0) + 8

  def allocateNamed(name: String, size: Int): RegisterAssignment = {
    val assign = allocate(size)
    symbols.put(name, assign)
    assign
  }

  def allocate(size: Int): RegisterAssignment = {
    stackOffset += size
    RegisterAssignment.Memory(Address.IndirectDisplacement(Register.rbp, -stackOffset))
  }

  def get(name: String): RegisterAssignment =
    symbols.get(name).get

}
