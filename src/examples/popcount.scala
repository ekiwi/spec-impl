// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>
package examples

import chisel3._
import chisel3.util._
import specimpl._


object popcount {
  def get_examples = Seq(
    (() => new CountLeadingZeros(8), true),
    (() => new CountLeadingZeros(16), true),
  )
}


class CountLeadingZerosIO(val wd_size: Int) extends Bundle {
  val ix_size = log2Ceil(wd_size)
  require(1 << ix_size == wd_size, s"wd_size=$wd_size is not a power of two!")
  val in = Input(UInt(wd_size.W))
  val out = Output(UInt(ix_size.W))
}

class CountLeadingZeros(wd_size: Int) extends Module {
  val io = IO(new CountLeadingZerosIO(wd_size))
  io.out := DontCare // remove an artifact of the equivalence checking

  def recursive(in: UInt): UInt = {
    if(in.getWidth == 2) {
      !in(1)
    } else {
      val lhs = in.head(in.getWidth/2)
      val rhs = in.tail(in.getWidth/2)
      val left_empty = !lhs.orR()
      left_empty ## recursive(Mux(left_empty, rhs, lhs))
    }
  }

  spec {
    // starting from the maximum number of zeros, check if there are i leading zeros
    val tests = (1 to wd_size).reverse.map{ i => (io.in.head(i) === 0.U) -> i.U }.toArray
    io.out := MuxCase(0.U, tests)
    // special case: if the input is zero, the output will be wd_size - 1
    when(io.in === 0.U) { io.out := (wd_size-1).U }
  } .impl {
    io.out := recursive(io.in)
  }
}