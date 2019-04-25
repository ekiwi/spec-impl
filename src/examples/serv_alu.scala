// Copyright 2019 The Regents of the University of California
// Copyright 2019 Olof Kindgren
// released under BSD 2-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>
// based on Olof Kindgren's implementation in Verilog


import chisel3._
import chisel3.util._

class cmp_io extends Bundle {
  val sel =  Input(UInt(1.W))
  val neg =  Input(Bool())
  val uns =  Input(Bool())
  val out =  Output(UInt(1.W))
}

class sh_io extends Bundle {
  val right =  Input(Bool())
  val signed =  Input(Bool())
  val done =  Output(Bool())
}

class rd_io extends Bundle {
  val sel =  Input(UInt(2.W))
  val out =  Output(UInt(1.W))
}

class serv_alu extends Module {
  val ResultAdd  = 0.U(2.W)
  val ResultSr   = 1.U(2.W)
  val ResultLt   = 2.U(2.W)
  val ResultBool = 3.U(2.W)
  val CmpLt = 0.U(1.W)
  val CmpEq = 1.U(1.W)

  val io = IO(new Bundle {
    val en = Input(Bool())
    val rs1 = Input(UInt(1.W))
    val op_b = Input(UInt(1.W))
    val buf = Input(UInt(1.W))
    val init = Input(Bool())
    val cnt_done = Input(Bool())
    val sub = Input(Bool())
    val bool_op = Input(UInt(2.W))
    val cmp = new cmp_io
    val shamt_en = Input(Bool())
    val sh = new sh_io
    val rd = new rd_io
  })

  val shamt_msb = Reg(UInt(1.W))
  val result_lt_r = Reg(UInt(1.W))
  val en_r = RegNext(io.en)
  val eq_r = Reg(Bool())

  val b_inv_plus_1 = Wire(UInt(1.W))
  val result_eq = eq_r && (io.rs1 === io.op_b)


  val shamt_reg = Module(new shift_reg(LEN = 5))
  shamt_reg.io.en := io.shamt_en
  shamt_reg.io.d := Mux(io.sh.right, io.op_b, b_inv_plus_1)

  val shift = Module(new ser_shift)
  shift.io.load := io.init
  shift.io.shamt := Cat(shamt_reg.io.par, shamt_reg.io.q)
  shift.io.shamt_msb := shamt_msb
  shift.io.signbit := io.sh.signed & io.rs1
  shift.io.right <> io.sh.right
  shift.io.done <> io.sh.done
  shift.io.d := io.buf

  val ser_add_inv_plus_1 = Module(new ser_add)
  ser_add_inv_plus_1.io.a := ~io.op_b
  ser_add_inv_plus_1.io.b := io.en & !en_r
  ser_add_inv_plus_1.io.clr := !io.en
  ser_add_inv_plus_1.io.q <> b_inv_plus_1
  when(io.shamt_en) {
    shamt_msb := ser_add_inv_plus_1.io.o_v
  }

  val ser_add_inst = Module(new ser_add)
  ser_add_inst.io.a := io.rs1
  ser_add_inst.io.b := Mux(io.sub, b_inv_plus_1, io.op_b)
  ser_add_inst.io.clr := !io.en

  val ser_lt_inst = Module(new ser_lt)
  ser_lt_inst.io.a := io.rs1
  ser_lt_inst.io.b := io.op_b
  ser_lt_inst.io.clr := !io.init
  ser_lt_inst.io.sign := io.cnt_done && !io.cmp.uns

  io.cmp.out := io.cmp.neg =/= Mux(io.cmp.sel === CmpEq, result_eq, ser_lt_inst.io.q)

  val BOOL_LUT = "h8e96".U
  val result_bool = BOOL_LUT(Cat(io.bool_op, io.rs1, io.op_b))

  io.rd.out := MuxLookup(io.rd.sel, 0.U, Seq(
    ResultAdd -> ser_add_inst.io.q,
    ResultSr -> shift.io.q,
    ResultLt -> result_lt_r,
    ResultBool -> result_bool
  ))

  when(io.init) {
    result_lt_r := ser_lt_inst.io.q
    eq_r := result_eq
  } .otherwise {
    eq_r := 1.U
    when(result_lt_r === 1.U) { // TODO: isn't this guard useless?
      result_lt_r := 0.U
    }
  }
}

class shift_reg(val LEN : Int = 0, val INIT : Int = 0) extends Module {
  val io = IO(new Bundle {
    val en = Input(Bool())
    val d = Input(UInt(1.W))
    val q = Output(UInt(1.W))
    val par = Output(UInt((LEN-1).W))
  })

  val data = RegInit(INIT.U(LEN.W))
  io.q := data.tail(1)
  io.par := data.head(LEN-1)
  when(io.en) {
    data := Cat(io.d, data.head(LEN-1))
  }
}

class ser_add_io extends Bundle {
  val a = Input(UInt(1.W))
  val b = Input(UInt(1.W))
  val clr = Input(Bool())
  val q = Output(UInt(1.W))
  val o_v = Output(UInt(1.W))
}

class ser_add extends Module {
  val io = IO(new ser_add_io)
  val c_r = RegInit(0.U(1.W))
  val axorb = io.a ^ io.b
  io.o_v := (axorb & c_r) | (io.a & io.b)
  io.q := axorb ^ c_r
  c_r := !io.clr & io.o_v
}

class ser_lt_io extends Bundle {
  val a = Input(UInt(1.W))
  val b = Input(UInt(1.W))
  val clr = Input(Bool())
  val sign = Input(Bool())
  val q = Output(UInt(1.W))
}

class ser_lt extends Module {
  val io = IO(new ser_lt_io)

  val lt_r = Reg(UInt(1.W))
  val lt = Mux(io.sign, io.a & ~io.b, ~io.a & io.b) | ((io.a === io.b) & lt_r)
  io.q := lt
  lt_r := lt & ~io.clr
}

class ser_shift extends Module {
  val io = IO(new Bundle {
    val load = Input(Bool())
    val shamt = Input(UInt(5.W))
    val shamt_msb = Input(UInt(1.W))
    val signbit = Input(UInt(1.W))
    val right = Input(Bool())
    val done = Output(Bool())
    val d = Input(UInt(1.W))
    val q = Output(UInt(1.W))
  })

  val cnt = Reg(UInt(6.W))
  val signbit = Reg(UInt(1.W))
  val wrapped = RegNext(cnt.head(1) | io.shamt_msb & !io.right)

  when(io.load) {
    cnt := 0.U
    signbit := io.signbit & io.right
  } .otherwise {
    cnt := cnt + 1.U
  }

  io.done := cnt.tail(5) === io.shamt
  io.q := Mux(io.right =/= wrapped, io.d, signbit)

}