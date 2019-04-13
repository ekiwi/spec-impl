// Copyright 2019 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>
package examples

import chisel3._
import chisel3.util._
import specimpl._

// some simple ALU-ish circuits to illustrate different concepts

object simple_alu {
  def get_examples = Seq(
    //(() => new CompleteSpec_MissingAdd, false),
    //(() => new CompleteSpec_Correct, true),
    (() => new IncompleteSpec_AlwaysOutput, false),
    (() => new IncompleteSpec_Correct, true)
  )
}

class ALUInterface(control_bits: Int, status_bits: Int = 0) extends Bundle {
  val a = Input(UInt(8.W))
  val b = Input(UInt(8.W))
  val c = Output(UInt(8.W))
  val ctrl = Input(UInt(control_bits.W))
  val status = Output(UInt(status_bits.W))
}

abstract class SimpleALUModule(control_bits: Int = 1, status_bits: Int = 0)  extends Module {
  val correct : Boolean
  val io = IO(new ALUInterface(control_bits, status_bits))
  io.c := 0.U
  io.status := 0.U
}

class CompleteSpec_MissingAdd extends SimpleALUModule {
  override val correct = false

  spec {
    switch(io.ctrl) {
      is(0.U) {
        io.c := io.a + io.b
      }
      is(1.U) {
        io.c := io.a - io.b
      }
    }
  } .impl {
    val is_add = io.ctrl === 0.U
    val is_sub = io.ctrl === 1.U
    // BUG: two's complement needs a +1!
    val b = Mux(is_sub, ~io.b, io.b)
    io.c := io.a + b
  }
}


class CompleteSpec_Correct extends SimpleALUModule {
  override val correct = true

  spec {
    switch(io.ctrl) {
      is(0.U) {
        io.c := io.a + io.b
      }
      is(1.U) {
        io.c := io.a - io.b
      }
    }
  } .impl {
    val is_add = io.ctrl === 0.U
    val is_sub = io.ctrl === 1.U
    val b = Mux(is_sub, ~io.b + 1.U, io.b)
    io.c := io.a + b
  }
}

class IncompleteSpec_AlwaysOutput extends SimpleALUModule(2) {
  // 2 control bits -> 4 possible inputs
  override val correct = false

  spec {
    switch(io.ctrl) {
      is(0.U) {
        io.c := io.a + io.b
      }
      is(1.U) {
        io.c := io.a - io.b
      }
    }
  } .impl {
    val is_add = io.ctrl === 0.U
    val is_sub = io.ctrl === 1.U
    val b = Mux(is_sub, ~io.b + 1.U, io.b)
    // BUG: spec does not assign to io.c iff io.ctrl in {2,3}
    io.c := io.a + b
  }
}

class IncompleteSpec_Correct extends SimpleALUModule(2) {
  // 2 control bits -> 4 possible inputs
  override val correct = false

  spec {
    switch(io.ctrl) {
      is(0.U) {
        io.c := io.a + io.b
      }
      is(1.U) {
        io.c := io.a - io.b
      }
    }
  } .impl {
    val is_add = io.ctrl === 0.U
    val is_sub = io.ctrl === 1.U
    val b = Mux(is_sub, ~io.b + 1.U, io.b)
    // FIX: only update io.c for ctrl we care about
    when(is_add || is_sub) { io.c := io.a + b }
  }
}
