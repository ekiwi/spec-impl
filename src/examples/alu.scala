// Copyright 2019 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>


package examples

import chisel3._
import chisel3.util._
import specimpl._

// This alu is based on Verilog code by Antti Lukats released under Apache-2.0 license
// https://github.com/micro-FPGA/engine-V
class mf8_alu extends Module {
  val io = IO(new Bundle {
    val ROM_Data = Input(UInt(16.W))
    val ROM_Pattern = Input(UInt(8.W))
    val A = Input(UInt(8.W))
    val B = Input(UInt(8.W))
    val Q = Output(UInt(8.W))
    val SREG = Input(UInt(8.W))
    val PassB = Input(Bool())
    val Skip = Input(Bool())
    val Do_Other = Output(Bool())
    val Z_Skip = Output(Bool())
    val Status_D = Output(UInt(7.W))
    val Status_Wr = Output(UInt(7.W))
  })

  // decoder
  val is_sbc = io.ROM_Data(15,10) === 0x2.U
  val is_add = io.ROM_Data(15,10) === 0x3.U
  val is_sub = io.ROM_Data(15,10) === 0x6.U
  val is_adc = io.ROM_Data(15,10) === 0x7.U
  val is_and = io.ROM_Data(15,10) === 0x8.U
  val is_xor = io.ROM_Data(15,10) === 0x9.U
  val is_or  = io.ROM_Data(15,10) === 0xa.U
  val is_mov = io.ROM_Data(15,10) === 0xb.U
  val is_imm_sub = io.ROM_Data(15,12) === 0x5.U
  val is_imm_or  = io.ROM_Data(15,12) === 0x6.U
  val is_imm_and = io.ROM_Data(15,12) === 0x7.U
  val is_io      = io.ROM_Data(15,12) === 0xb.U
  val is_ldi     = io.ROM_Data(15,12) === 0xe.U
  val is_uop = io.ROM_Data(15,9) === 0x4a.U
  val maybe_swap = io.ROM_Data(3,0) === 0x2.U
  val maybe_asr  = io.ROM_Data(3,0) === 0x5.U
  val maybe_lsr  = io.ROM_Data(3,0) === 0x6.U
  val maybe_ror  = io.ROM_Data(3,0) === 0x7.U

  spec {
    when(is_sbc) {
      io.Q := io.A - io.B
    }

  } .impl {

    // add/sub unit
    val addsub_inst = Module(new addsub8())
    addsub_inst.io.a := io.A
    addsub_inst.io.b := io.B

    // do_X registers
    val Do_DO = !io.PassB && !io.Skip
    val Do_SUB = RegNext(Do_DO && (is_sub || is_sbc || is_imm_sub))
    val Do_ADD = RegNext(Do_DO && (is_add || is_adc))
    val Use_Carry = RegNext(Do_DO && (is_sbc || is_adc))
    val Do_AND = RegNext(Do_DO && (is_and || is_imm_and))
    val Do_XOR = RegNext(Do_DO && (is_xor))
    val Do_OR = RegNext(Do_DO && (is_or || is_imm_or))
    val Do_SWAP = RegNext(Do_DO && (is_uop && maybe_swap))
    val Do_ASR = RegNext(Do_DO && (is_uop && maybe_asr))
    val Do_LSR = RegNext(Do_DO && (is_uop && maybe_lsr))
    val Do_ROR = RegNext(Do_DO && (is_uop && maybe_ror))
    val Do_SBRC = RegNext(Do_DO && (io.ROM_Data(15, 9) === 0x7e.U))
    val Do_SBRS = RegNext(Do_DO && (io.ROM_Data(15, 9) === 0x7f.U))
    val Do_PASSB = RegNext(io.PassB || (!io.Skip && (is_mov || is_io || is_ldi)))
    // control add/sub unit
    addsub_inst.io.sub := Do_SUB
    addsub_inst.io.cin := Do_SUB ^ Use_Carry ^ io.SREG(0)
    // Do_Other
    io.Do_Other := Do_PASSB

    // result
    when(Do_ADD || Do_SUB) {
      io.Q := addsub_inst.io.q // Q_v
    }.elsewhen(Do_AND) {
      io.Q := io.A & io.B // Q_L
    }.elsewhen(Do_OR) {
      io.Q := io.A | io.B // Q_L
    }.elsewhen(Do_XOR) {
      io.Q := io.A ^ io.B // Q_L
    }.elsewhen(Do_SWAP) {
      io.Q := Cat(io.A(3, 0), io.A(7, 4)) // Q_S
    }.otherwise {
      val msb = io.A(7) & Do_ASR | io.SREG(0) & Do_ROR
      io.Q := Cat(msb, io.A(7, 1)) // Q_R
    }

    // status wr
    val status_on = Do_ASR || Do_LSR || Do_ROR || Do_SUB || Do_ADD || Do_AND || Do_XOR || Do_OR
    val Status_Wr = RegNext(Mux(status_on, 3.U(7.W), 0.U(7.W)))
    io.Status_Wr := Status_Wr

    // status: zero
    val is_zero = (io.Q === 0.U)
    val d0 = Wire(UInt(1.W))
    when(is_zero && (!Do_SUB || !Use_Carry)) {
      d0 := 1.U
    }.elsewhen(is_zero && Do_SUB && Use_Carry) {
      d0 := io.SREG(1)
    }.otherwise {
      d0 := 0.U
    }
    // status: carry out
    val d1 = ((addsub_inst.io.cout ^ Do_SUB) & (Do_ADD || Do_SUB)) | (io.A(0) & (Do_ASR || Do_LSR || Do_ROR))
    io.Status_D := Cat(0.U(6.W), d1, d0)

    // Z Skip (for Skip If Bit Set or Skip If Bit Clear instruction)
    val bit_set = (io.ROM_Pattern & io.A) =/= 0.U
    val bit_clear = (io.ROM_Pattern & io.A) === 0.U
    io.Z_Skip := (bit_set && Do_SBRS) || (bit_clear && Do_SBRC)
  }
}


class addsub8 extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(8.W))
    val b = Input(UInt(8.W))
    val q = Output(UInt(8.W))
    val sub = Input(Bool())
    val cin = Input(UInt(1.W))
    val cout = Output(UInt(1.W))
  })

  val b : UInt = Mux(io.sub, ~io.b, io.b)
  val res = (io.a +& b) + io.cin
  io.q := res(7, 0)
  io.cout := res(8)
}