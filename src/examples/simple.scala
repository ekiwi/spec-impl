// Copyright 2019 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

import chisel3._
import chisel3.util._
import specimpl._

class SimpleALU(correct: Boolean) extends  Module {
  val io = IO(new Bundle {
    val a = Input(UInt(8.W))
    val b = Input(UInt(8.W))
    val c = Output(UInt(8.W))
    val ctrl = Input(UInt(3.W))
    val status = Output(UInt(2.W))
  })

  io.c := 0.U
  io.status := 0.U

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
    val b = if(correct) {
      Mux(is_sub, (~io.b) + 1.U, io.b)
    } else {
      Mux(is_sub, ~io.b, io.b)
    }
    when(is_add || is_sub) {
      io.c := io.a + b
    }
  }

}
